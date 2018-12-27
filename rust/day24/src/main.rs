#[macro_use]
extern crate lazy_static;
extern crate regex;

use std::cmp;
use std::cmp::Ordering;
use std::fs;
use regex::Regex;


#[derive(Clone, Copy, Debug, PartialEq)]
enum Team {
    Infection,
    ImmuneSystem
}


#[derive(Clone, Debug, PartialEq)]
enum AttackType {
    Radiation,
    Bludgeoning,
    Fire,
    Slashing,
    Cold
}


#[derive(Clone, Debug, PartialEq)]
struct Group {
    team: Team,
    num_units: i32,
    hit_points: i32,
    attack_damage: i32,
    attack_type: AttackType,
    initiative: i32,
    weaknesses: Vec<AttackType>,
    immunities: Vec<AttackType>
}


fn effective_power(g: &Group) -> i32  {
    g.num_units * g.attack_damage
}


fn inflicted_damage(attacker: &Group, defender: &Group) -> i32 {
    if defender.immunities.contains(&attacker.attack_type) {
        0
    } else if defender.weaknesses.contains(&attacker.attack_type) {
        effective_power(attacker) * 2
    } else {
        effective_power(attacker)
    }
}


fn fight(groups: &mut Vec<Group>) {
    //.Phase 1: Target selection
    let mut selected_targets: Vec<(usize,usize)> = Vec::new();
    {
        let mut indices: Vec<usize> = (0..groups.len()).collect();
        indices.sort_by_key(|&i| (cmp::Reverse(effective_power(&groups[i])),
                                  cmp::Reverse(groups[i].initiative)));

        let (immunes, infections): (Vec<usize>, Vec<usize>) = indices.iter().partition(|&i| groups[*i].team == Team::ImmuneSystem);

        for i in indices.iter() {
            let targets: &Vec<usize> = match groups[*i].team {
                Team::ImmuneSystem => &infections,
                Team::Infection    => &immunes,
            };

            let target = targets
                .iter()
                .filter(|&j| selected_targets.iter().all(|(_,d)| d != j))
                .max_by_key(|&j| (inflicted_damage(&groups[*i], &groups[*j]),
                                  effective_power(&groups[*j]),
                                  groups[*j].initiative));

            if let Some(j) = target {
                selected_targets.push((*i, *j));
            }
        }
    }

    // Phase 2: Attacking
    selected_targets.sort_by_key(|&(a,_)| cmp::Reverse(groups[a].initiative));
    for (i, j) in selected_targets {
        let dealt_damage = inflicted_damage(&groups[i], &groups[j]);
        let units_killed = cmp::min(groups[j].num_units, dealt_damage / groups[j].hit_points);
        groups[j].num_units -= units_killed;
    }
    groups.retain(|g| g.num_units > 0);
}


fn combat(groups: &mut Vec<Group>) -> Option<Team> {
    let mut prev_num_units: i32 = groups.iter().map(|g| g.num_units).sum();
    while groups.iter().any(|g| g.team != groups[0].team) {
        fight(groups);

        let num_units: i32 = groups.iter().map(|g| g.num_units).sum();
        if num_units == prev_num_units {
            return None;
        }
        prev_num_units = num_units;
    }

    if groups.is_empty() {
        None
    } else {
        Some(groups[0].team)
    }
}


fn parse_attacktype(s: &str) -> AttackType {
    match s {
        "radiation"   => AttackType::Radiation,
        "bludgeoning" => AttackType::Bludgeoning,
        "fire"        => AttackType::Fire,
        "slashing"    => AttackType::Slashing,
        "cold"        => AttackType::Cold,
        other         => panic!("Unknown attack type {}", other),
    }
}


fn parse_group(s: &str, team: &Team) -> Option<Group> {
    lazy_static! {
        static ref group_rx: Regex = Regex::new(r"^(\d+) units each with (\d+) hit points( | \(.*\) )with an attack that does (\d+) ([^ ]+) damage at initiative (\d+)$").unwrap();
        static ref immune_rx: Regex = Regex::new(r"immune to ([a-z]+(?:, [a-z]+)*)").unwrap();
        static ref weak_rx: Regex = Regex::new(r"weak to ([a-z]+(?:, [a-z]+)*)").unwrap();
    }

    if let Some(caps) = group_rx.captures(s) {
        let mut immunities = match immune_rx.captures(&caps[3]) {
            Some(immune_caps) => immune_caps[1].split(", ").map(parse_attacktype).collect(),
            None              => Vec::new()
        };

        let mut weaknesses = match weak_rx.captures(&caps[3]) {
            Some(weak_caps) => weak_caps[1].split(", ").map(parse_attacktype).collect(),
            None            => Vec::new()
        };

        Some(Group {
            team: *team,
            num_units: caps[1].parse().unwrap(),
            hit_points: caps[2].parse().unwrap(),
            attack_damage: caps[4].parse().unwrap(),
            attack_type: parse_attacktype(&caps[5]),
            initiative: caps[6].parse().unwrap(),
            weaknesses: weaknesses,
            immunities: immunities,
        })
    } else {
        None
    }
}


fn parse(s: &str) -> Vec<Group> {
    let mut result = Vec::new();
    let mut current_team = Team::ImmuneSystem;
    for line in s.lines() {
        if line == "Immune System:" {
            current_team = Team::ImmuneSystem;
        } else if line == "Infection:" {
            current_team = Team::Infection;
        } else if let Some(group) = parse_group(&line, &current_team) {
            result.push(group);
        }
    }
    result
}


fn part_one(groups: &Vec<Group>) -> i32 {
    let mut groups = groups.clone();
    combat(&mut groups);
    groups.iter().map(|g| g.num_units).sum()
}


fn boost_immune_system(groups: &Vec<Group>, attack_damage_boost: i32) -> Vec<Group> {
    groups
        .iter()
        .cloned()
        .map(|g| match g.team {
            Team::ImmuneSystem => Group { attack_damage: g.attack_damage + attack_damage_boost, .. g },
            Team::Infection    => g,
        })
        .collect()
}


fn part_two(groups: &Vec<Group>) -> i32 {
    let smallest_winning_boost = (0..100_000)
        .collect::<Vec<i32>>()
        .binary_search_by(|&boost| match combat(&mut boost_immune_system(groups, boost)) {
            Some(Team::ImmuneSystem) => Ordering::Greater,
            _                        => Ordering::Less
        })
        .unwrap_err() as i32;

    part_one(&boost_immune_system(groups, smallest_winning_boost))
}


fn main() {
    let input = fs::read_to_string("input.txt").unwrap();

    let groups = parse(&input);

    println!("Part one: {}", part_one(&groups));
    println!("Part two: {}", part_two(&groups));
}

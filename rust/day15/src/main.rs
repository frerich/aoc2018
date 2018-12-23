use std::fs;
use std::collections::HashSet;
use std::{thread, time};

const UNIT_INITIAL_HEALTH: i32 = 200;
const UNIT_ATTACK_POWER: i32   = 3;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
struct Position {
    x: i32,
    y: i32
}

#[derive(Clone, Debug, PartialEq)]
enum Race {
    Elf,
    Goblin
}

#[derive(Clone, Debug)]
struct Unit {
    race: Race,
    hit_points: i32,
    attack_power: i32,
    position: Position
}

enum Square {
    Wall,
    Open,
    Mark(char)
}

struct Cave {
    squares: Vec<Vec<Square>>,
    units: Vec<Unit>
}

impl Position {
    fn adjacent(self: &Self) -> Vec<Self> {
        vec![
            Position { x: self.x, y: self.y - 1 },
            Position { x: self.x, y: self.y + 1 },
            Position { x: self.x - 1, y: self.y },
            Position { x: self.x + 1, y: self.y }
        ]
    }
}

impl Unit {
    fn new(race: Race, position: Position) -> Self {
        Unit {
            race: race,
            hit_points: UNIT_INITIAL_HEALTH,
            attack_power: UNIT_ATTACK_POWER,
            position: position
        }
    }
}

impl Cave {
    fn is_open(self: &Self, p: &Position) -> bool {
        if let Some(row) = self.squares.get(p.y as usize) {
            if let Some(Square::Open) = row.get(p.x as usize) {
                return !self.units.iter().any(|u| &u.position == p);
            }
        }
        false
    }
}

fn parse(s: &str) -> Cave {
    let mut units = Vec::new();
    let mut squares = Vec::new();
    for (y, line) in s.lines().enumerate() {
        let mut row = Vec::new();
        for (x, ch) in line.chars().enumerate() {
            match ch {
                '#' => row.push(Square::Wall),
                '.' => row.push(Square::Open),
                'G' => { row.push(Square::Open); units.push(Unit::new(Race::Goblin, Position { x: x as i32, y: y as i32 })) },
                'E' => { row.push(Square::Open); units.push(Unit::new(Race::Elf, Position { x: x as i32, y: y as i32 })) },
                _   => {}
            }
        }
        squares.push(row);
    }
    Cave { squares, units }
}

fn render(cave: &Cave) -> String {
    let mut result = String::new();
    for (y, row) in cave.squares.iter().enumerate() {
        for (x, square) in row.iter().enumerate() {
            match square {
                Square::Mark(c) => result.push(*c),
                Square::Wall    => result.push('#'),
                Square::Open    =>
                    match cave.units.iter().find(|u| u.position == Position { x: x as i32, y: y as i32 }) {
                        Some(Unit { race: Race::Goblin, .. } ) => result.push('G'),
                        Some(Unit { race: Race::Elf, .. } )    => result.push('E'),
                        None                                   => result.push('.')
                    }
            }
        }
        result.push('\n');
    }
    result
}

fn play_round(cave: &mut Cave) -> bool {
    const DEBUG: bool = false;

    let mut dead_unit_idx: HashSet<usize> = HashSet::new();

    let mut round_completed: bool = true;

    cave.units.sort_by_key(|unit| (unit.position.y, unit.position.x));
    for i in 0..cave.units.len() {
        if DEBUG { println!("Turn of unit {}/{} starts", i, cave.units.len()-1); }
        if dead_unit_idx.contains(&i) {
            if DEBUG { println!("  This unit is dead -- nothing to do."); }
            continue;
        }

        let unit: Unit = cave.units[i].clone();

        if DEBUG { println!("  This unit is a {:?}", unit); }

        let target_idx: Vec<usize> = cave.units
            .iter()
            .enumerate()
            .filter_map(|(i,u)| if !dead_unit_idx.contains(&i) && unit.race != u.race { Some(i) } else { None })
            .collect();
        if target_idx.is_empty() {
            if DEBUG { println!("  No enemies left! Aborting round."); }
            round_completed = false;
            break;
        }

        let mut clash_idx: Vec<usize> = target_idx
            .iter()
            .cloned()
            .filter(|i| unit.position.adjacent().contains(&cave.units[*i].position))
            .collect();
        if DEBUG { println!("  This unit has {} targets, {} of which are right next to it", target_idx.len(), clash_idx.len()); }

        // Nothing close by, let's move first
        if clash_idx.is_empty() {
            if DEBUG { println!("  Since this unit does not clash with anything, it moves"); }
            // Find open squares which are in range of enemies
            let in_range_squares = target_idx
                .iter()
                .map(|i| cave.units[*i].position.adjacent())
                .flatten()
                .filter(|p| cave.is_open(&p))
                .collect::<HashSet<Position>>();
            if DEBUG { println!("  There are {} squares in range of enemies", in_range_squares.len()); }

            // Find all paths to all in range squares
            if DEBUG { println!("  Computing paths to in-range squares..."); }
            let mut paths = shortest_paths(&cave, &unit.position, &in_range_squares.iter().cloned().collect::<HashSet<Position>>());
            if DEBUG { println!("  Identified {} paths", paths.len()); }
            paths.sort_by_key(|path| path.len());

            // Found at least one walkable path?
            if !paths.is_empty() {
                // Are there multiple shortest paths?
                let shortest_paths = paths
                    .iter()
                    .take_while(|path| path.len() == paths[0].len());

                let new_pos = shortest_paths
                    .map(|path| path[1])
                    .min_by_key(|pos| (pos.y, pos.x))
                    .expect("ERGS");
                if DEBUG { println!("  Moving unit {} to {:?}", i, new_pos); }
                cave.units[i].position = new_pos;

                clash_idx = target_idx
                    .iter()
                    .cloned()
                    .filter(|j| cave.units[i].position.adjacent().contains(&cave.units[*j].position))
                    .collect();
            }
        }

        // Now, let's see if we clash with anything
        if DEBUG { println!("  Unit clashes with {} enemies", clash_idx.len()); }
        if !clash_idx.is_empty() {
            // There's an enemy in front! Let's pick the weakest one..
            clash_idx.sort_by_key(|i| cave.units[*i].hit_points);

            let weakest_unit_idx: usize = clash_idx
                .iter()
                .cloned()
                .take_while(|i| cave.units[*i].hit_points == cave.units[clash_idx[0]].hit_points)
                .min_by_key(|i| (cave.units[*i].position.y, cave.units[*i].position.x))
                .unwrap();

            if DEBUG { println!("  Attacking unit {} ({:?})", weakest_unit_idx, cave.units[weakest_unit_idx]); }

            cave.units[weakest_unit_idx].hit_points -= unit.attack_power;
            if DEBUG { println!("  Unit {} now has {} hit points left", weakest_unit_idx, cave.units[weakest_unit_idx].hit_points ); }
            if cave.units[weakest_unit_idx].hit_points <= 0 {
                dead_unit_idx.insert(weakest_unit_idx);
            }
        }
    }

    // Eliminate all units which died during this round
    cave.units = cave.units
        .iter()
        .cloned()
        .enumerate()
        .filter_map(|(i,u)| if dead_unit_idx.contains(&i) { None } else { Some(u) })
        .collect();

    round_completed
}

fn explore(cave: &Cave, paths: &Vec<Vec<Position>>, visited: &mut HashSet<Position>) -> Vec<Vec<Position>> {
    let mut result: Vec<Vec<Position>> = Vec::new();
    for path in paths {
        let last_square: &Position = &path[path.len() - 1];
        for pos in last_square.adjacent() {
            if !visited.contains(&pos) && cave.is_open(&pos) {
                let mut new_path: Vec<Position> = path.clone();
                new_path.push(pos);
                result.push(new_path);
                visited.insert(pos);
            }
        }
    }
    result
}

fn shortest_paths(cave: &Cave, a: &Position, bs: &HashSet<Position>) -> Vec<Vec<Position>> {
    let mut visited: HashSet<Position> = HashSet::new();
    let mut paths: Vec<Vec<Position>> = vec![vec![*a]];
    visited.insert(*a);

    loop {
        let solutions: Vec<Vec<Position>> = paths
            .iter()
            .filter(|path| bs.contains(&path[path.len() - 1]))
            .cloned()
            .collect();
        if !solutions.is_empty() {
            return solutions;
        }

        paths = explore(&cave, &paths, &mut visited);
        if paths.is_empty() {
            return Vec::new();
        }
    }
}


fn part_one(input: &str) -> usize {
    const DEBUG: bool = false;

    let mut cave = parse(&input);

    for round_no in 0.. {
        if DEBUG {
            thread::sleep(time::Duration::from_millis(100));
            print!("{}[2J", 27 as char);
            print!("After {} rounds:\n{}", round_no, render(&cave));
            cave.units.sort_by_key(|u| (u.position.y, u.position.x));
            for u in &cave.units {
                print!("{:?} @ {}/{} ({} HP), ", u.race, u.position.x, u.position.y, u.hit_points);
            }
            println!("\n\nPlaying round {}... \n", round_no + 1);
        }
        if !play_round(&mut cave) {
            println!("*** Combat ends! {} rounds completed", round_no);
            println!("*** Total health of remaining units: {}", cave.units.iter().map(|u| u.hit_points).sum::<i32>());
            return round_no * cave.units.into_iter().map(|u| u.hit_points).sum::<i32>() as usize;
        }
    }
    0
}


fn main() -> Result<(), std::io::Error> {
    let input = fs::read_to_string("input.txt")?;
    println!("Part one: {}", part_one(&input));

    Ok(())
}


#[cfg(test)]
mod tests {
    use super::part_one;

    #[test]
    fn combat_one() {
        let input =
            "#######\n\
             #.G...#\n\
             #...EG#\n\
             #.#.#G#\n\
             #..G#E#\n\
             #.....#\n\
             #######";
        assert_eq!(part_one(input), 27730);
    }

    #[test]
    fn combat_two() {
        let input =
            "#######\n\
             #G..#E#\n\
             #E#E.E#\n\
             #G.##.#\n\
             #...#E#\n\
             #...E.#\n\
             #######";
        assert_eq!(part_one(input), 36334);
    }

    #[test]
    fn combat_three() {
        let input =
            "#######\n\
             #E..EG#\n\
             #.#G.E#\n\
             #E.##E#\n\
             #G..#.#\n\
             #..E#.#\n\
             #######";
        assert_eq!(part_one(input), 39514);
    }

    #[test]
    fn combat_four() {
        let input =
            "#######\n\
             #E.G#.#\n\
             #.#G..#\n\
             #G.#.G#\n\
             #G..#.#\n\
             #...E.#\n\
             #######";
        assert_eq!(part_one(input), 27755);
    }

    #[test]
    fn combat_five() {
        let input =
            "#######\n\
             #.E...#\n\
             #.#..G#\n\
             #.###.#\n\
             #E#G#G#\n\
             #...#G#\n\
             #######";
        assert_eq!(part_one(input), 28944);
    }

    #[test]
    fn combat_six() {
        let input =
            "#########\n\
             #G......#\n\
             #.E.#...#\n\
             #..##..G#\n\
             #...##..#\n\
             #...#...#\n\
             #.G...G.#\n\
             #.....G.#\n\
             #########";
        assert_eq!(part_one(input), 18740);
    }
}


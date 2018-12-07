use std::collections::HashMap;
use std::collections::HashSet;
use std::fs::File;
use std::io::BufReader;
use std::io::BufRead;
use std::iter::FromIterator;


type Step = char;
type Dependencies = HashMap<Step, Vec<Step>>;


fn main() {
    let input_file = File::open("input.txt").unwrap();

    let lines = BufReader::new(&input_file)
        .lines()
        .map(|line| line.unwrap())
        .collect();

    let deps = parse(&lines);

    println!("Part one: {:?}", String::from_iter(part_one(&deps)));
}

fn parse(lines: &Vec<String>) -> Dependencies {
    let mut deps = Dependencies::new();
    for line in lines {
        let step_name = line.chars().nth(5).unwrap();
        let next_step_name = line.chars().nth(36).unwrap();

        // TODO: Insert with default in one go.
        deps.entry(step_name).or_default();
        deps.get_mut(&step_name).unwrap().push(next_step_name);
    }
    deps
}

fn find_first_steps(deps: &Dependencies) -> Vec<Step> {
    // TODO: chain iterators
    let mut all_next_steps: HashSet<Step> = HashSet::new();
    for (step_name, next_step_names) in deps {
        for next_step_name in next_step_names {
            all_next_steps.insert(*next_step_name);
        }
    }

    // TODO: Maybe we can return an iterator directly?
    deps.keys().filter(|step| !all_next_steps.contains(step)).map(|step| *step).collect()
}

fn find_dependencies(step: Step, deps: &Dependencies) -> Vec<Step>
{
    deps
        .iter()
        .filter_map(|(step_name, next)| if next.contains(&step) { Some(*step_name) } else { None })
        .collect()
}

fn is_ready(step: Step, completed_steps: &Vec<Step>, deps: &Dependencies) -> bool {
    find_dependencies(step, deps).iter().all(|step| completed_steps.contains(step))
}

fn part_one(deps: &Dependencies) -> Vec<Step> {
    let mut result: Vec<Step> = Vec::new();

    let mut steps = find_first_steps(&deps);

    while !steps.is_empty() {
        // TODO It would be better to not check result.contains all the time!
        let next_ready_step = steps.iter().filter(|step| !result.contains(step) && is_ready(**step, &result, deps)).map(|step| *step).min();
        match next_ready_step {
            Some(step) => {
                result.push(step);
                if let Some(step_deps) = deps.get(&step) {
                    steps.extend(step_deps);
                }
            }
            None => break
        }
    }
    result
}


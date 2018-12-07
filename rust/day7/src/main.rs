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

    println!("Part one: {}", String::from_iter(part_one(&deps)));
    println!("Part two: {}", part_two(&deps));
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
    for next_step_names in deps.values() {
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

fn part_two(deps: &Dependencies) -> u32 {
    let mut result = 0;
    let mut workers: Vec<(Step,u8)> = Vec::new();
    let mut finished_steps: Vec<Step> = Vec::new();
    let mut step_queue: Vec<Step> = find_first_steps(&deps);

    while !step_queue.is_empty() || !workers.is_empty() {
        println!("tick {}: step_queue = {:?}, workers: {:?}", result, step_queue, workers);
        let (mut busy_workers, finished_workers) : (Vec<(Step,u8)>, Vec<(Step,u8)>) = workers.iter().partition(|(_step, time_left)| *time_left > 1);
        for (_step, time_left) in & mut busy_workers {
            *time_left -= 1;
        }

        for (step, _time_left) in &finished_workers {
            finished_steps.push(*step);
            if let Some(step_deps) = deps.get(&step) {
                step_queue.extend(step_deps.iter().filter(|s| !finished_steps.contains(s)));
                step_queue.sort();
                step_queue.dedup();
            }
        }

        workers = busy_workers;

        while workers.len() < 5 && !step_queue.is_empty() {
            // println!("have only {} workers, want to dispatch next item from step_queue {:?}, finished_steps: {:?}", workers.len(), step_queue, finished_steps);
            let next_ready_step = step_queue.iter().filter(|step| is_ready(**step, &finished_steps, deps)).map(|step| *step).min();
            if let Some(step) = next_ready_step {
                // println!("dispatching step {}", step);
                step_queue.retain(|s| s != &step);
                let duration = step as u8 - 'A' as u8 + 61;
                workers.push((step, duration));
            } else {
                // println!("no further step is ready!");
                break;
            }
        }

        result = result + 1;
    }

    result - 1
}

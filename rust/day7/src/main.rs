use std::collections::HashMap;
use std::collections::HashSet;
use std::fs::File;
use std::io::BufReader;
use std::io::BufRead;
use std::iter::FromIterator;


type Step = char;

// The Depndencies map associates each step with the set of next steps.
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


// Parse the puzzle data into a Dependency table
fn parse(lines: &Vec<String>) -> Dependencies {
    let mut deps = Dependencies::new();
    for line in lines {
        let step_name = line.chars().nth(5).unwrap();
        let next_step_name = line.chars().nth(36).unwrap();

        deps
            .entry(step_name)
            .or_insert(Vec::new())
            .push(next_step_name);
    }
    deps
}


// Given a dependency table, yield the initial steps (i.e. those which are not
// successors of any steps).
fn initial_steps(deps: &Dependencies) -> Vec<Step> {
    let all_next_steps: HashSet<&Step> = deps.values().flatten().collect();

    deps
        .keys()
        .filter_map(|step| if all_next_steps.contains(step) { None } else { Some(*step) })
        .collect()
}


// Yield the steps which have to precede the given step, i.e. the prerequisites
fn preceding_steps(step: &Step, deps: &Dependencies) -> Vec<Step>
{
    deps
        .iter()
        .filter_map(|(step_name, next)| if next.contains(step) { Some(*step_name) } else { None })
        .collect()
}


// Indicates whether the given step is ready: it is 'readyy' when all preceding steps are complete.
fn is_ready(step: &Step, completed_steps: &Vec<Step>, deps: &Dependencies) -> bool {
    preceding_steps(step, deps).iter().all(|step| completed_steps.contains(step))
}


fn part_one(deps: &Dependencies) -> Vec<Step> {
    let mut result: Vec<Step> = Vec::new();

    let mut steps: HashSet<Step> = initial_steps(&deps).iter().cloned().collect();

    loop {
        let next_ready_step = steps.iter().filter(|step| is_ready(step, &result, deps)).cloned().min();
        match next_ready_step {
            Some(step) => {
                steps.remove(&step);
                result.push(step);
                if let Some(next_steps) = deps.get(&step) {
                    steps.extend(next_steps);
                }
            }
            None => break
        }
    }
    result
}


fn part_two(deps: &Dependencies) -> u32 {
    const NUM_WORKERS: usize = 5;

    let mut num_ticks = 0;
    let mut workers: Vec<(Step,u8)> = Vec::new();
    let mut finished_steps: Vec<Step> = Vec::new();
    let mut step_queue: HashSet<Step> = initial_steps(&deps).iter().cloned().collect();

    while !step_queue.is_empty() || !workers.is_empty() {
        // Tick down all workers
        for (_step, time_left) in & mut workers {
            *time_left -= 1
        }

        // Identify busy workers (which require more time) and finished ones
        let (busy_workers, finished_workers) : (Vec<(Step,u8)>,Vec<(Step,u8)>)
            = workers.iter().partition(|(_step, time_left)| *time_left > 0);

        // We'll continiue workiing with the busy workers
        workers = busy_workers;

        // For the finished workers, check which step they processed and queue the next steps
        for (step, _time_left) in finished_workers {
            finished_steps.push(step);
            if let Some(next_steps) = deps.get(&step) {
                step_queue.extend(next_steps.iter().filter(|s| !finished_steps.contains(s)));
            }
        }

        // Try to keep all workers busy
        while workers.len() < NUM_WORKERS && !step_queue.is_empty() {
            let next_ready_step = step_queue.iter().filter(|step| is_ready(step, &finished_steps, deps)).cloned().min();
            match next_ready_step {
                Some(step) => {
                    step_queue.remove(&step);
                    let duration = step as u8 - 'A' as u8 + 61;
                    workers.push((step, duration));
                }
                None => break
            }
        }
        num_ticks += 1
    }

    num_ticks - 1
}

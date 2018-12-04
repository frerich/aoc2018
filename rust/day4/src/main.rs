#[macro_use]
extern crate lazy_static;
extern crate regex;
extern crate chrono;
extern crate counter;


use chrono::{Duration, NaiveDateTime, Timelike};
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::collections::HashSet;
use std::str::FromStr;
use regex::Regex;
use counter::Counter;


#[derive(Debug)]
enum Event {
    ShiftStart(i32),
    Asleep,
    WakeUp
}


#[derive(Debug)]
struct Record {
    timestamp: NaiveDateTime,
    event: Event
}


#[derive(Debug, Clone)]
struct Shift {
    id: i32,
    asleep: Vec<(NaiveDateTime, NaiveDateTime)>
}


impl FromStr for Record {
    type Err = String;

    // TODO Error handling
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        lazy_static! {
            static ref shift_start_rx: Regex = Regex::new(r"Guard #(\d+) begins shift").unwrap();
        }

        let timestamp = &s[1..17];
        let timestamp = NaiveDateTime::parse_from_str(timestamp, "%Y-%m-%d %H:%M").unwrap();

        let event = &s[19..];
        let event =
            if event == "falls asleep" {
                Event::Asleep
            } else if event == "wakes up" {
                Event::WakeUp
            } else {
                let cap = shift_start_rx.captures(event).unwrap();
                Event::ShiftStart(cap[1].parse().unwrap())
            };

        Ok(Record { timestamp: timestamp, event: event })
    }
}


fn reconstruct_shifts(records: &Vec<Record>) -> Vec<Shift> {
    let mut result = Vec::<Shift>::new();
    let mut shift: Shift = Shift { id: -1, asleep: Vec::<(NaiveDateTime, NaiveDateTime)>::new() };
    let mut sleep: (NaiveDateTime, NaiveDateTime) = (NaiveDateTime::from_timestamp(0,0), NaiveDateTime::from_timestamp(0,0));
    for record in records {
        match record.event {
            Event::ShiftStart(id) => {
                if &shift.id != &-1 {
                    result.push(shift.clone());
                }
                shift.id = id;
                shift.asleep.clear();
            }
            Event::Asleep => {
                sleep.0 = record.timestamp;
            }
            Event::WakeUp => {
                sleep.1 = record.timestamp; 
                shift.asleep.push(sleep);
            }
        }
    }
    result.push(shift);
    result
}


fn part_one(records: &Vec<Record>) -> i32
{
    // TODO: Drop notion of Shits in favor of sleep phases
    let shifts = reconstruct_shifts(&records);

    // Build a map telling which guard was asleep on which minute
    let mut asleep_phases: HashMap<i32, Vec<HashSet<u32>>> = HashMap::new();
    for shift in &shifts {
        let mut asleep_mins: HashSet<u32> = HashSet::new();
        for (start, end) in &shift.asleep {
            for min in start.minute() .. end.minute() {
                asleep_mins.insert(min);
            }
        }

        match asleep_phases.entry(shift.id) {
            Entry::Vacant(e) => { e.insert(vec![asleep_mins]); }
            Entry::Occupied(mut e) => { e.get_mut().push(asleep_mins); }
        }
    }

    // Identify the sleepiest guard by counting how long every guard was asleep
    //. TODO: Replace with asleep_phases.max_by_key
    let mut max_mins_asleep: usize = 0;
    let mut sleepiest_guard_id: i32 = 0;
    for (id, sleep_phases) in &asleep_phases {
        let mins_asleep: usize = sleep_phases.iter().map(|shift| shift.len()).sum();
        if mins_asleep > max_mins_asleep {
            max_mins_asleep = mins_asleep;
            sleepiest_guard_id = *id;
        }
    }

    // Figure out on which minute the sleepiest guard was most often asleep.
    let mut counts: Counter<_> = Counter::new();
    let phases: Vec<HashSet<u32>> = asleep_phases.get(&sleepiest_guard_id).unwrap().clone();
    for phase in &phases {
        counts += phase.iter().cloned().collect::<Counter<_>>();
    }
    let most_common_minute: i32 = counts.most_common_ordered().get(0).unwrap().0 as i32;

    return sleepiest_guard_id * most_common_minute;
}


fn part_two(records: &Vec<Record>) -> i32 {
    // TODO: Drop notion of Shits in favor of sleep phases
    let shifts = reconstruct_shifts(&records);

    // Build a map telling which guard was asleep on which minute
    let mut asleep_phases: HashMap<i32, Vec<HashSet<u32>>> = HashMap::new();
    for shift in &shifts {
        let mut asleep_mins: HashSet<u32> = HashSet::new();
        for (start, end) in &shift.asleep {
            for min in start.minute() .. end.minute() {
                asleep_mins.insert(min);
            }
        }

        match asleep_phases.entry(shift.id) {
            Entry::Vacant(e) => { e.insert(vec![asleep_mins]); }
            Entry::Occupied(mut e) => { e.get_mut().push(asleep_mins); }
        }
    }

    // Identify the sleepiest guard by counting how long every guard was asleep
    //. TODO: Replace with asleep_phases.max_by_key
    let mut max_repeated_minute: usize = 0;
    let mut result: i32 = 0;
    for (id, sleep_phases) in &asleep_phases {
        let mut counts: Counter<_> = Counter::new();
        for phase in sleep_phases.iter() {
            counts += phase.iter().cloned().collect::<Counter<_>>();
        }

        if let Some(most_common) = counts.most_common_ordered().get(0) {
            if most_common.1 > max_repeated_minute {
                max_repeated_minute = most_common.1;
                result = id * most_common.0 as i32;
            }
        }
    }

    result
}

fn main() {
    let input_file = File::open("input.txt").unwrap();

    let mut records: Vec<Record>
        = BufReader::new(&input_file)
        .lines()
        .map(|line| line.unwrap().parse().unwrap())
        .collect();
    records.sort_by(|a, b| a.timestamp.cmp(&b.timestamp));

    println!("Part one: {:?}", part_one(&records));
    println!("Part two: {:?}", part_two(&records));
}

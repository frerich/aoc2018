use std::io::BufRead;
use std::io::BufReader;
use std::fs::File;
use std::collections::HashSet;


fn part_one(frequency_changes: &Vec<i32>) -> i32 {
    frequency_changes.iter().sum()
}


fn part_two(frequency_changes: &Vec<i32>) -> i32 {
    let mut current_freq: i32 = 0;
    let mut seen: HashSet<i32> = HashSet::new();
    for freq in frequency_changes.iter().cycle() {
        current_freq += freq;
        if seen.contains(&current_freq) {
            break;
        }
        seen.insert(current_freq);
    }
    current_freq
}


fn main() {
    let input_file = File::open("input.txt").unwrap();

    let frequency_changes: Vec<i32> =
        BufReader::new(&input_file)
        .lines()
        .map(|line| line.unwrap().parse().unwrap())
        .collect();

    println!("Part one: {:?}", part_one(&frequency_changes));
    println!("Part two: {:?}", part_two(&frequency_changes));
}

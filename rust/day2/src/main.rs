extern crate counter;

use std::io::BufRead;
use std::io::BufReader;
use std::fs::File;
use counter::Counter;


fn part_one(ids: &Vec<String>) -> usize {
    let histograms: Vec<Counter<_>> = ids.iter().map(|id| id.chars().collect()).collect();

    let doublettes = histograms.iter().filter(
        |&h| h.values().any(|&value| value == 2)
    );

    let triplets = histograms.iter().filter(
        |&h| h.values().any(|&value| value == 3)
    );

    &doublettes.count() * &triplets.count()
}


fn main() {
    let input_file = File::open("input.txt").unwrap();

    let ids = BufReader::new(&input_file)
                .lines()
                .map(|line| line.unwrap())
                .collect();

    println!("Part one: {:?}", part_one(&ids));
}

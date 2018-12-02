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


fn pairs<T>(v: &Vec<T>) -> Vec<(&T,&T)> {
    let mut result = vec![];
    for (i, x) in v.iter().enumerate() {
        for y in v[i+1..].iter() {
            result.push((x,y));
        }
    }
    result
}


fn part_two(ids: &Vec<String>) -> String
{
    for (a,b) in pairs(ids) {
        let pairwise_chars: Vec<(char,char)> = a.chars().zip(b.chars()).collect();

        let different_chars = pairwise_chars.iter().filter(|(x,y)| x != y);
        if different_chars.count() == 1 {
            return pairwise_chars.iter().filter(|(x,y)| x == y).map(|(x,y)| x).collect();
        }
    }
    String::new()
}


fn main() {
    let input_file = File::open("input.txt").unwrap();

    let ids = BufReader::new(&input_file)
                .lines()
                .map(|line| line.unwrap())
                .collect();

    println!("Part one: {:?}", part_one(&ids));
    println!("Part two: {:?}", part_two(&ids));
}

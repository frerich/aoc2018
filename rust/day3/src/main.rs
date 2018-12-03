#[macro_use]
extern crate lazy_static;
extern crate regex;
extern crate counter;

use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::str::FromStr;
use regex::Regex;
use counter::Counter;


#[derive(Debug)]
struct Rectangle {
    id: i32,
    x: i32,
    y: i32,
    width: i32,
    height: i32
}

impl FromStr for Rectangle {
    type Err = String;

    // TODO: Add error handling for parse errors
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        lazy_static! {
            static ref rx: Regex = Regex::new(r"^#(\d+) @ (\d+),(\d+): (\d+)x(\d+)$").unwrap();
        }
        let cap = rx.captures(s).unwrap();
        Ok(Rectangle { id: cap[1].parse().unwrap(), x: cap[2].parse().unwrap(), y: cap[3].parse().unwrap(), width: cap[4].parse().unwrap(), height: cap[5].parse().unwrap() })
    }
}


// Gives all the cells covered by some rectangle
fn cells(r: &Rectangle) -> Vec<(i32,i32)> {
    let mut result = Vec::<(i32,i32)>::new();
    for y in r.y .. r.y + r.height {
        for x in r.x .. r.x + r.width {
            result.push((x,y));
        }
    }
    result
}


fn part_one(rects: &Vec<Rectangle>) -> usize
{
    let counts: Counter<_> = rects.iter().flat_map(cells).collect();
    counts.values().filter(|&v| v >= &2).count()
}


fn part_two(rects: &Vec<Rectangle>) -> i32
{
    let counts: Counter<_> = rects.iter().flat_map(cells).collect();
    for rect in rects.iter() {
        if cells(rect).iter().all(|pos| counts.get(pos) == Some(&1)) {
            return rect.id;
        }
    }
    0
}


fn main() {
    let input_file = File::open("input.txt").unwrap();

    let rects: Vec<Rectangle>
            = BufReader::new(&input_file)
            .lines()
            .map(|line| line.unwrap().parse().unwrap())
            .collect();

    println!("Part one: {:?}", part_one(&rects));
    println!("Part two: {:?}", part_two(&rects));
}

#[macro_use]
extern crate lazy_static;
extern crate regex;


use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use std::str::FromStr;
use regex::Regex;


#[derive(Debug, Clone)]
struct Point {
    x: i32,
    y: i32,
    vx: i32,
    vy: i32
}


impl FromStr for Point {
    type Err = String;

    // TODO: Add error handling for parse errors
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        lazy_static! {
            static ref rx: Regex = Regex::new(r"position=<\s*(-?\d+),\s*(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+)>").unwrap();
        }
        let cap = rx.captures(s).unwrap();
        Ok(Point { x: cap[1].parse().unwrap(), y: cap[2].parse().unwrap(), vx: cap[3].parse().unwrap(), vy: cap[4].parse().unwrap() })
    }
}


fn advance(points: &mut Vec<Point>) {
    for point in points.iter_mut() {
        point.x += point.vx;
        point.y += point.vy;
    }
}


fn bounding_rect(points: &Vec<Point>) -> ((i32,i32), (i32,i32)) {
    let min_x = points.iter().map(|p| p.x).min().unwrap();
    let max_x = points.iter().map(|p| p.x).max().unwrap();
    let min_y = points.iter().map(|p| p.y).min().unwrap();
    let max_y = points.iter().map(|p| p.y).max().unwrap();
    ((min_x,min_y), (max_x,max_y))
}


fn render(points: &Vec<Point>) -> String {
    let ((min_x,min_y), (max_x,max_y)) = bounding_rect(&points);

    let mut s = String::new();

    for y in min_y ..= max_y {
        for x in min_x ..= max_x {
            if points.iter().any(|p| p.x == x && p.y == y) {
                s.push('#')
            } else {
                s.push('.')
            }
        }
        s.push('\n');
    }

    s
}

fn main() {
    let input_file = File::open("input.txt").unwrap();

    let mut points: Vec<Point> = BufReader::new(&input_file)
        .lines()
        .map(|line| line.unwrap().parse().unwrap())
        .collect();

    let mut prev_area: i64 = -1;
    let mut prev_points: Vec<Point> = Vec::new();

    for sec in 0.. {
        let ((min_x,min_y), (max_x,max_y)) = bounding_rect(&points);

        let area: i64 = (max_x - min_x) as i64 * (max_y - min_y) as i64;

        if prev_area > 0 && prev_area < area {
            println!("{} seconds:\n{}", sec, render(&prev_points));
            break;
        }
        prev_area = area;
        prev_points = points.clone();

        advance(&mut points);
    }
}

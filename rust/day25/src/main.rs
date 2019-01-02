use std::fs;
use std::collections::HashSet;
use std::iter;


type Point = (i32,i32,i32,i32);


fn neighbors(p: Point, distance: u32) -> HashSet<Point> {
    if distance == 0 {
        iter::once(p).collect()
    } else {
        let mut result = HashSet::new();
        for q in neighbors(p, distance - 1) {
            result.insert(q);
            result.insert((q.0,     q.1,     q.2,     q.3 + 1));
            result.insert((q.0,     q.1,     q.2,     q.3 - 1));
            result.insert((q.0,     q.1,     q.2 + 1, q.3));
            result.insert((q.0,     q.1,     q.2 - 1, q.3));
            result.insert((q.0,     q.1 + 1, q.2,     q.3));
            result.insert((q.0,     q.1 - 1, q.2,     q.3));
            result.insert((q.0 + 1, q.1,     q.2,     q.3));
            result.insert((q.0 - 1, q.1,     q.2,     q.3));
        }
        result
    }
}


fn constellations(points: &Vec<Point>) -> Vec<HashSet<Point>> {
    let mut points = points.clone();
    let mut result = Vec::new();

    while !points.is_empty() {
        let mut constellation = neighbors(points.pop().unwrap(), 3);
        loop {
            let (connected, disconnected) : (Vec<_>, Vec<_>) = points.iter().partition(|p| constellation.contains(&p));
            if connected.is_empty() {
                break;
            }
            for p in connected {
                constellation.extend(neighbors(p, 3));
            }
            points = disconnected;
        }
        result.push(constellation)
    }

    result
}


fn parse(input: &str) -> Vec<Point> {
    input
        .lines()
        .map(|line| { let mut nums = line.split(","); (nums.next().unwrap().parse().unwrap(), nums.next().unwrap().parse().unwrap(), nums.next().unwrap().parse().unwrap(), nums.next().unwrap().parse().unwrap()) } )
        .collect()
}


fn part_one(points: &Vec<Point>) -> usize {
    constellations(points).len()
}


fn main() {
    let input = fs::read_to_string("input.txt").unwrap();

    let points = parse(&input);

    println!("Part one: {}", part_one(&points));
}

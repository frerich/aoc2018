use std::fs;


type Point = (i32,i32,i32,i32);


fn distance(p: &Point, q: &Point) -> i32 {
    (q.0 - p.0).abs() + (q.1 - p.1).abs() + (q.2 - p.2).abs() + (q.3 - p.3).abs()
}


fn constellations(points: &Vec<Point>) -> Vec<Vec<Point>> {
    let mut points = points.clone();
    let mut result = Vec::new();

    while !points.is_empty() {
        let mut constellation = Vec::new();
        constellation.push(points.pop().unwrap());

        loop {
            let (connected, disconnected) : (Vec<_>, Vec<_>) = points
                .iter()
                .partition(|p| constellation.iter().any(|q| distance(p, q) <= 3));

            if connected.is_empty() {
                break;
            }

            constellation.extend(connected);
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


fn main() {
    let input = fs::read_to_string("input.txt").unwrap();

    let points = parse(&input);

    println!("Part one: {}", constellations(&points).len());
}

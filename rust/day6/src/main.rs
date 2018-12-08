use std::collections::HashMap;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;


type Coord = (i32,i32);


fn parse_line(line: &str) -> Coord {
    let v: Vec<&str> = line.split(", ").collect();
    (v[0].parse().unwrap(), v[1].parse().unwrap())
}


fn manhattan_distance(a: &Coord, b: &Coord) -> i32 {
    let dx = a.0 - b.0;
    let dy = a.1 - b.1;
    dx.abs() + dy.abs()
}


fn bounding_rect(coords: &Vec<Coord>) -> (i32,i32,i32,i32) {
    let xs: Vec<i32> = coords.iter().map(|(x,_)| *x).collect();
    let ys: Vec<i32> = coords.iter().map(|(_,y)| *y).collect();

    let min_x = xs.iter().min().unwrap_or(&0);
    let min_y = ys.iter().min().unwrap_or(&0);
    let max_x = xs.iter().max().unwrap_or(&0);
    let max_y = ys.iter().max().unwrap_or(&0);

    (*min_x, *min_y, *max_x, *max_y)
}


fn part_one(coords: &Vec<Coord>) -> usize {
    let mut areas: HashMap<&Coord, Vec<Coord>> = HashMap::new();

    // For each cell of the bounding rect, determine the nearest input coordinate
    let (min_x, min_y, max_x, max_y) = bounding_rect(coords);
    for y in min_y ..= max_y {
        for x in min_x ..= max_x {
            let nearest: &Coord = coords.iter().min_by(|a,b| manhattan_distance(a,&(x,y)).cmp(&manhattan_distance(b,&(x,y)))).unwrap();
            areas.entry(nearest).or_insert(Vec::new()).push((x,y));
        }
    }

    // Identify finite areas by excluding those which touch the edges.
    let on_edge = |(x,y)| x == min_x || x == max_x || y == min_y || y == max_y;
    let finite_areas = areas.iter().filter(|(_, cs)| !cs.iter().any(|c| on_edge(*c)));

    // Yield the size of the largest area.
    finite_areas
        .map(|(_, cs)| cs.len())
        .max()
        .unwrap()
}


fn part_two(coords: &Vec<Coord>) -> usize {
    let mut safe_region_size: usize = 0;

    let (min_x, min_y, max_x, max_y) = bounding_rect(coords);
    for y in min_y ..= max_y {
        for x in min_x ..= max_x {
            let is_safe = coords.iter().map(|c| manhattan_distance(c, &(x,y))).sum::<i32>() < 10000;
            if is_safe {
                safe_region_size += 1;
            }
        }
    }

    safe_region_size
}


fn main() {
    let input_file = File::open("input.txt").unwrap();

    let coords: Vec<Coord> = BufReader::new(&input_file)
        .lines()
        .map(|line| parse_line(&line.unwrap()))
        .collect();

    println!("Part one: {:?}", part_one(&coords));
    println!("Part two: {:?}", part_two(&coords));
}

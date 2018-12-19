use std::collections::HashMap;
use std::fs;


#[derive(PartialEq)]
enum Cell {
    FlowingWater,
    RestingWater,
    Clay
}


fn parse(input: &str) -> HashMap<(i32,i32), Cell> {
    let mut result = HashMap::new();

    for line in input.lines() {
        let mut tokens = line.split(", ");
        let token_a = tokens.next().unwrap();
        let token_b = tokens.next().unwrap();
        let val_a: i32 = token_a[2..].parse().unwrap();
        let mut bounds = token_b[2..].split("..");
        let min_bound: i32 = bounds.next().unwrap().parse().unwrap();
        let max_bound: i32 = bounds.next().unwrap().parse().unwrap();
        for i in min_bound ..= max_bound {
            if line.starts_with("x") {
                result.insert((val_a, i), Cell::Clay);
            } else {
                result.insert((i, val_a), Cell::Clay);
            }
        }
    }

    result
}


#[allow(dead_code)]
fn render(area: &HashMap<(i32, i32), Cell>) {
    let min_x = area.keys().map(|(x,_)| *x).min().expect("finding min x in render");
    let min_y = area.keys().map(|(_,y)| *y).min().expect("finding min y in render");
    let max_x = area.keys().map(|(x,_)| *x).max().expect("finding max x in render");
    let max_y = area.keys().map(|(_,y)| *y).max().expect("finding max y in render");

    println!("Top left = {},{}", min_x, min_y);
    for y in min_y ..= max_y {
        for x in min_x ..= max_x {
            match area.get(&(x,y)) {
                Some(Cell::FlowingWater) => print!("|"),
                Some(Cell::RestingWater) => print!("~"),
                Some(Cell::Clay)         => print!("#"),
                None                     => print!(".")
            }
        }
        print!("\n");
    }
}


fn flood_horizontally(x: &mut i32, dx: i32, y: i32, area: &mut HashMap<(i32, i32), Cell>) -> bool {
    loop {
        if let Some(Cell::Clay) = area.get(&(*x + dx, y)) {
            break;
        }
        *x += dx;
        area.insert((*x, y), Cell::FlowingWater);
        if !area.contains_key(&(*x, y + 1)) {
            pour(*x, y + 1, area);
            match area.get(&(*x + dx, y + 1)) {
                Some(Cell::FlowingWater) => return true,
                None                     => return true,
                _                        => {}
            }
        }
    }
    false
}


fn pour(spring_x: i32, spring_y: i32, area: &mut HashMap<(i32, i32), Cell>) {
    let max_y = area.keys().map(|(_,y)| *y).max().unwrap_or_default();

    // First, pour down until something other than sand is hit
    let mut y = spring_y;
    while y <= max_y && !area.contains_key(&(spring_x, y)) {
        area.insert((spring_x, y), Cell::FlowingWater);
        y += 1;
    }

    // We poured all the way to the bottom, stop working
    if y > max_y {
        return;
    }

    // We poured into flowing water (i.e. streams merge), we're done
    if let Some(Cell::FlowingWater) = area.get(&(spring_x, y)) {
        return;
    }

    y -= 1;

    // Now, try to fill the reservoir up
    while y >= spring_y {
        let mut overflows: bool = false;

        // Flood to the left
        let mut min_x = spring_x;
        if flood_horizontally(&mut min_x, -1, y, area) {
            overflows = true;
        }

        // Flood to the right
        let mut max_x = spring_x;
        if flood_horizontally(&mut max_x, 1, y, area) {
            overflows = true;
        }

        // If there are open ends on either side, we're done.
        if overflows {
            break;
        }

        // Otherwise, declare the entire row resting water and move up
        for x in min_x ..= max_x {
            area.insert((x, y), Cell::RestingWater);
        }

        y -= 1;
    }
}


fn main() {
    let input: String = fs::read_to_string("input.txt").unwrap();

    let mut area = parse(&input);
    let min_y = area.keys().map(|(_,y)| *y).min().expect("Figure out minimum Y value of clay in pour");
    let max_y = area.keys().map(|(_,y)| *y).max().expect("Figure out maximum Y value of clay in pour");

    pour(500, 0, &mut area);

    let cells_in_range: Vec<Cell> = area
        .into_iter()
        .filter_map(|((_, y), c)| if y >= min_y && y <= max_y { Some(c) } else { None })
        .collect();
    let num_flowing_cells = cells_in_range.iter().filter(|c| **c == Cell::FlowingWater).count();
    let num_resting_cells = cells_in_range.iter().filter(|c| **c == Cell::RestingWater).count();

    println!("Part one: {}", num_flowing_cells + num_resting_cells);
    println!("Part two: {}", num_resting_cells);
}

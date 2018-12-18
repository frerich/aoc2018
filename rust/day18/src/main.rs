use std::fs;


#[derive(PartialEq)]
enum Acre {
    Open,
    Trees,
    Lumberyard
}

type Area = Vec<Vec<Acre>>;


fn at(area: &Area, x: i32, y: i32) -> Option<&Acre> {
    if let Some(row) = area.get(y as usize) {
        row.get(x as usize)
    } else {
        None
    }
}


fn adjacent(x: i32, y: i32, area: &Area) -> Vec<&Acre> {
    let neighbors =
        [(-1,-1),(0,-1),(1,-1),
        (-1, 0),       (1, 0),
        (-1, 1),(0, 1),(1, 1)];

    neighbors
        .iter()
        .filter_map(|(dx, dy)| at(area, x + dx, y + dy))
        .collect()
}


fn parse_acre(ch: char) -> Acre {
    match ch {
        '.' => Acre::Open,
        '|' => Acre::Trees,
        '#' => Acre::Lumberyard,
        _   => Acre::Open
    }
}


fn parse(input: &str) -> Area {
    input
        .lines()
        .map(|line| line.chars().map(parse_acre).collect())
        .collect()
}


fn evolve_acre(area: &Area, x: i32, y: i32) -> Acre {
    let adjacent = adjacent(x, y, &area);
    match area[y as usize][x as usize] {
        Acre::Open => {
            if adjacent.into_iter().filter(|a| **a == Acre::Trees).count() >= 3 {
                Acre::Trees
            } else {
                Acre::Open
            }
        }

        Acre::Trees => {
            if adjacent.into_iter().filter(|a| **a == Acre::Lumberyard).count() >= 3 {
                Acre::Lumberyard
            } else {
                Acre::Trees
            }
        }

        Acre::Lumberyard => {
            if adjacent.contains(&&Acre::Lumberyard) && adjacent.contains(&&Acre::Trees) {
                Acre::Lumberyard
            } else {
                Acre::Open
            }
        }
    }
}


fn evolve(area: &Area) -> Area {
    let mut result = Vec::new();
    for (y, row) in area.iter().enumerate() {
        let mut new_row = Vec::new();
        for (x, _acre) in row.iter().enumerate() {
            new_row.push(evolve_acre(&area, x as i32, y as i32));
        }
        result.push(new_row);
    }
    result
}


fn resource_value(area: &Area) -> usize {
    let acres: Vec<&Acre> = area.iter().flatten().collect();
    let num_wooded = acres.iter().filter(|a| ***a == Acre::Trees).count();
    let num_lumberyards = acres.iter().filter(|a| ***a == Acre::Lumberyard).count();

    num_wooded * num_lumberyards
}


fn part_one(input: &str) -> usize {
    let mut area = parse(&input);
    for _ in 1..=10 {
        area = evolve(&area);
    }
    resource_value(&area)
}


fn part_two(input: &str) -> usize {
    const OFFSET: usize = 426;
    const PERIOD: usize = 28;

    let mut area = parse(&input);
    let min = OFFSET + (1_000_000_000 - OFFSET) % PERIOD;
    for _ in 1..=min {
        area = evolve(&area);
    }

    resource_value(&area)
}


fn main() -> Result<(), std::io::Error> {
    let input: String = fs::read_to_string("input.txt")?;

    println!("Part one: {}", part_one(&input));
    println!("Part two: {}", part_two(&input));

    Ok(())
}

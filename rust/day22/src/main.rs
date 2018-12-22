use std::cmp;


type GeologicalIndex = usize;
type Cave = Vec<Vec<GeologicalIndex>>;


const DEPTH: usize = 3879;
const TARGET: (usize,usize) = (8, 713);


fn erosion_level(cave: &Cave, x: usize, y: usize) -> usize {
    (cave[y][x] + DEPTH) % 20183
}


fn geological_index(cave: &Cave, x: usize, y: usize) -> GeologicalIndex {
    match (x,y) {
        (0,0)  => 0,
        TARGET => 0,
        (x,0)  => x * 16807,
        (0,y)  => y * 48271,
        (x,y)  => erosion_level(cave, x - 1, y) * erosion_level(cave, x, y - 1)
    }
}


fn cave() -> Cave {
    let mut result = Vec::new();

    let max_dim = cmp::max( TARGET.0, TARGET.1 );
    for _ in 0..max_dim + 1 {
        let mut v = Vec::new();
        v.resize(max_dim + 1, 0);
        result.push(v);
    }

    for y in 0 .. max_dim + 1 {
        for x in 0 .. max_dim + 1 {
            result[y][x] = geological_index(&result, x, y);
        }
    }

    result
}


fn part_one() -> usize {
    let cave = cave();

    let mut result: usize = 0;
    for y in 0 ..= TARGET.1 {
        for x in 0 ..= TARGET.0 {
            result += erosion_level(&cave, x, y) % 3;
        }
    }

    result
}


fn main() {
    println!("Part one: {}", part_one());
}

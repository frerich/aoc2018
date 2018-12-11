type Coord = (i32,i32);


const SERIAL_NUMBER: i32 = 3628;
const GRID_SIZE: usize = 300;


fn power_level(coord: &Coord) -> i32 {
    let rack_id = coord.0 + 10;

    let mut result = rack_id * coord.1;
    result += SERIAL_NUMBER;
    result *= rack_id;
    result = (result / 100) % 10;
    result - 5
}

fn initial_grid() -> Vec<i32> {
    let mut grid: Vec<i32> = Vec::with_capacity(GRID_SIZE * GRID_SIZE);
    for y in 0 .. GRID_SIZE {
        for x in 0 .. GRID_SIZE {
            grid.push(power_level(&(x as i32,y as i32)));
        }
    }
    grid
}

fn largest_power_level(largest_square_size: usize) -> (Coord,usize) {
    let mut max_square_size = 1;
    let mut max_coord: Coord = (0,0);
    let mut max_power = power_level(&max_coord);

    let initial_grid = initial_grid();

    let mut current_grid: Vec<i32> = initial_grid.clone();

    for square_size in 2 ..= largest_square_size {
        let current_grid_size = GRID_SIZE - (square_size - 1) + 1;

        let mut new_grid: Vec<i32> = Vec::with_capacity((current_grid_size - 1) * (current_grid_size - 1));
        for y in 0 .. GRID_SIZE - square_size + 1 {
            for x in 0 .. GRID_SIZE - square_size + 1 {
                let mut power = current_grid[y * current_grid_size + x];
                for i in 0 .. square_size {
                    let val = initial_grid[(y + i) * GRID_SIZE + (x + square_size - 1)];
                    power += val;
                }
                for i in 0 .. square_size - 1 {
                    let val = initial_grid[(y + square_size - 1) * GRID_SIZE + (x + i)];
                    power += val;
                }

                new_grid.push(power);

                if power > max_power {
                    max_power = power;
                    max_coord = (x as i32,y as i32);
                    max_square_size = square_size;
                }

            }
        }
        current_grid = new_grid;
    }

    (max_coord, max_square_size)
}


fn part_one() -> Coord {
    largest_power_level(3).0
}


fn part_two() -> (Coord,usize) {
    largest_power_level(GRID_SIZE)
}


fn main() {
    println!("Part one: {:?}", part_one());
    println!("Part two: {:?}", part_two());
}

extern crate counter;


use std::fs;
use std::iter::FromIterator;
use std::cmp::Ordering;
use std::collections::HashSet;
use counter::Counter;


#[derive(Debug, PartialEq, Eq, Clone)]
enum Turn {
    Left, Straight, Right
}


#[derive(Debug, PartialEq, Eq, Clone)]
enum Direction {
    North, West, South, East
}


#[derive(Debug, Clone)]
struct Cart {
    pos: (usize,usize),
    direction: Direction,
    next_turn: Turn
}


impl PartialOrd for Cart {
    fn partial_cmp(&self, other: &Cart) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}


impl Ord for Cart {
    fn cmp(&self, other: &Cart) -> Ordering {
        if self.pos.1 != other.pos.1 {
            self.pos.1.cmp(&other.pos.1)
        } else {
            self.pos.0.cmp(&other.pos.0)
        }
    }
}


impl PartialEq for Cart {
    fn eq(&self, other: &Cart) -> bool {
        self.pos == other.pos
    }
}


impl Eq for Cart { }


fn find_carts(grid: &mut Vec<Vec<char>>) -> Vec<Cart>
{
    let mut carts: Vec<Cart> = Vec::new();

    for (y, row) in grid.iter_mut().enumerate() {
        for (x, cell) in row.iter_mut().enumerate() {
            if !"<>^v".contains(*cell) {
                continue;
            }

            let pos = (x,y);
            let direction = match *cell {
                '<' => Direction::West,
                '>' => Direction::East,
                '^' => Direction::North,
                'v' => Direction::South,
                _   => Direction::West // :-(
            };
            let next_turn = Turn::Left;

            carts.push(Cart { pos, direction, next_turn });

            *cell = if "<>".contains(*cell) { '-' } else { '|' };
        }
    }

    carts
}


#[allow(dead_code)]
fn render(grid: &Vec<Vec<char>>, carts: &Vec<Cart>) {
    let mut populated_grid = grid.clone();
    for cart in carts {
        match cart.direction {
            Direction::West  => populated_grid[cart.pos.1][cart.pos.0] = '<',
            Direction::East  => populated_grid[cart.pos.1][cart.pos.0] = '>',
            Direction::North => populated_grid[cart.pos.1][cart.pos.0] = '^',
            Direction::South => populated_grid[cart.pos.1][cart.pos.0] = 'v',
        }
    }

    for row in populated_grid.iter() {
        println!("{}", String::from_iter(row.iter()));
    }
}


fn turn_left(cart: &mut Cart) {
    cart.direction = match cart.direction {
        Direction::North => Direction::West,
        Direction::West => Direction::South,
        Direction::South => Direction::East,
        Direction::East => Direction::North
    }
}


fn turn_right(cart: &mut Cart) {
    turn_left(cart);
    turn_left(cart);
    turn_left(cart);
}


fn advance(grid: &Vec<Vec<char>>, cart: &mut Cart) {
    match cart.direction {
        Direction::West  => cart.pos.0 -= 1,
        Direction::East  => cart.pos.0 += 1,
        Direction::North => cart.pos.1 -= 1,
        Direction::South => cart.pos.1 += 1
    }

    match grid[cart.pos.1][cart.pos.0] {
        '+' => match cart.next_turn {
            Turn::Left => {
                turn_left(cart);
                cart.next_turn = Turn::Straight;
            }
            Turn::Straight => {
                cart.next_turn = Turn::Right;
            }
            Turn::Right => {
                turn_right(cart);
                cart.next_turn = Turn::Left;
            }
        }
        '\\' =>
            if cart.direction == Direction::East || cart.direction == Direction::West {
                turn_right(cart);
            } else {
                turn_left(cart);
            }
        '/' =>
            if cart.direction == Direction::East || cart.direction == Direction::West {
                turn_left(cart);
            } else {
                turn_right(cart);
            }
        _ => {}
    }
}


fn part_one(grid: &Vec<Vec<char>>, initial_carts: &Vec<Cart>) -> (usize,usize)
{
    let mut carts = initial_carts.clone();
    loop {
        carts.sort();
        let mut new_cart_positions: HashSet<(usize,usize)> = HashSet::new();
        for cart in carts.iter_mut() {
            if new_cart_positions.contains(&cart.pos) {
                return cart.pos;
            }
            advance(grid, cart);
            if new_cart_positions.contains(&cart.pos) {
                return cart.pos;
            }
            new_cart_positions.insert(cart.pos.clone());
        }
    }
}


fn part_two(grid: &Vec<Vec<char>>, initial_carts: &Vec<Cart>) -> (usize,usize)
{
    let mut carts = initial_carts.clone();
    while carts.len() > 1 {
        carts.sort();
        let mut new_cart_positions: HashSet<(usize,usize)> = HashSet::new();
        for cart in carts.iter_mut() {
            if new_cart_positions.contains(&cart.pos) {
                continue;
            }
            advance(grid, cart);
            new_cart_positions.insert(cart.pos.clone());
        }

        let counts: Counter<(usize,usize)> = carts.iter().map(|c| c.pos).collect();
        carts = carts
            .into_iter()
            .filter(|c| counts.get(&c.pos).unwrap() == &1)
            .collect();
    }
    carts[0].pos
}


fn main() {
    let mut grid: Vec<Vec<char>> =
        fs::read_to_string("input.txt").unwrap()
        .lines()
        .map(|line| Vec::from_iter(line.chars()))
        .collect();

    let carts = find_carts(&mut grid);

    println!("Part one: {:?}", part_one(&grid, &carts));
    println!("Part two: {:?}", part_two(&grid, &carts));
}

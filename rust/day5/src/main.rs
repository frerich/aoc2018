use std::fs::File;
use std::io::Read;
use std::collections::HashSet;


type Unit = char;
type Polymer = String;


fn can_react(a: Unit, b: Unit) -> bool {
    return a.to_uppercase().to_string() == b.to_uppercase().to_string() && a != b;
}


fn reduce(p: &Polymer) -> Polymer {
    let mut reduced: Vec<Unit> = Vec::new();

    for u in p.chars() {
        if !reduced.is_empty() && can_react(reduced[reduced.len() - 1], u) {
            reduced.pop();
        } else {
            reduced.push(u);
        }
    }

    reduced.iter().collect()
}


fn part_one(p: &Polymer) -> usize {
    reduce(p).len()
}


fn part_two(p: &Polymer) -> usize {
    let used_units: HashSet<Unit> = p.to_uppercase().chars().collect();
    used_units
        .iter()
        .map(|unit| p.chars().filter(|v| v.to_ascii_uppercase() != *unit).collect::<Polymer>())
        .map(|candidiate| reduce(&candidiate).len())
        .min()
        .unwrap_or(0)
}


fn main() {
    let mut input_file = File::open("input.txt").unwrap();

    let mut polymer = String::new();
    input_file.read_to_string(& mut polymer);

    println!("Part one: {}", part_one(&polymer));
    println!("Part two: {}", part_two(&polymer));
}

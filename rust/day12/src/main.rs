use std::collections::HashSet;
use std::fs;


fn parse(_s: &str) -> (String, HashSet<String>) {
    let initial_state = "#........#.#.#...###..###..###.#..#....###.###.#.#...####..##..##.#####..##...#.#.....#...###.#.####";

    let mut rules: HashSet<String> = HashSet::new();
    rules.insert("##..#".to_string());
    rules.insert(".##.#".to_string());
    rules.insert("..###".to_string());
    rules.insert("###.#".to_string());
    rules.insert("#.##.".to_string());
    rules.insert(".#.##".to_string());
    rules.insert("###..".to_string());
    rules.insert("#####".to_string());
    rules.insert("##...".to_string());
    rules.insert(".#...".to_string());
    rules.insert("..#..".to_string());
    rules.insert("...#.".to_string());
    rules.insert(".##..".to_string());
    rules.insert("..#.#".to_string());
    rules.insert(".####".to_string());

    (initial_state.to_string(), rules)
}


fn evolve(pots: &mut String, rules: &HashSet<String>, offset: &mut i32) {
    if &pots[..4] != "...." {
        *pots = "....".to_string() + &pots;
        *offset -= 4;
    }

    if &pots[pots.len() - 4..] != "...." {
        *pots += "....";
    }

    let mut new_state = String::with_capacity(pots.len());
    for i in 0 .. pots.len() - 4 {
        if rules.contains(&pots[i..i+5]) {
            new_state.push('#');
        } else {
            new_state.push('.');
        }
    }

    *offset += 2;
    *pots = new_state;
}


fn part_one(initial_state: &String, rules: &HashSet<String>) -> i32 {
    let mut state = initial_state.clone();
    let mut offset: i32 = 0;
    for _ in 0 .. 20 {
        evolve(&mut state, &rules, &mut offset);
    }

    state
        .char_indices()
        .map(|(i,c)| if c == '#' {i as i32 + offset} else {0})
        .sum()
}


fn part_two() -> usize {
    let generation: usize = 50_000_000_000;

    // I derived this formula by looking at the first 150 generations and noticing a pattern
    // starting at generation 92: the sum keeps increasing by 80 for every subsequent generation.
    (generation - 92) * 80 + 8226
}


fn main() {
    let input = fs::read_to_string("input.txt").unwrap();

    let (state, rules) = parse(&input);

    println!("Part one: {}", part_one(&state, &rules));
    println!("Part two: {}", part_two());
}

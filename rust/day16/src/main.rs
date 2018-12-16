use std::fs;
use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::hash_map::Entry;


#[derive(Debug)]
struct Sample {
    in_regs: [usize; 4],
    opcode: u8,
    args: [usize; 3],
    out_regs: [usize; 4]
}


fn exec(opcode: u8, args: &[usize; 3], regs: &mut [usize; 4]) {
    let [a, b, c] = *args;
    regs[c] = match opcode {
        0 => regs[a] + regs[b],
        1 => regs[a] + b,
        2 => regs[a] * regs[b],
        3 => regs[a] * b,
        4 => regs[a] & regs[b],
        5 => regs[a] & b,
        6 => regs[a] | regs[b],
        7 => regs[a] | b,
        8 => regs[a],
        9 => a,
        10 => if a > regs[b] { 1 } else { 0 },
        11 => if regs[a] > b { 1 } else { 0 },
        12 => if regs[a] > regs[b] { 1 } else { 0 },
        13 => if a == regs[b] { 1 } else { 0 },
        14 => if regs[a] == b { 1 } else { 0 },
        15 => if regs[a] == regs[b] { 1 } else { 0 },
        _ => 0
    }
}


fn matching_opcodes(sample: &Sample) -> Vec<u8> {
    let mut matches = Vec::new();
    for opcode in 0..16 {
        let mut regs = sample.in_regs.clone();
        exec(opcode, &sample.args, &mut regs);
        if regs == sample.out_regs {
            matches.push(opcode as u8);
        }
    }
    matches
}


fn parse_samples() -> Vec<Sample> {
    let mut samples: Vec<Sample> = Vec::new();

    let input = fs::read_to_string("input-part1.txt").unwrap();
    let mut iter = input.lines();
    loop {
        let before = iter.next().unwrap();
        if before.is_empty() {
            break;
        }
        let instr = iter.next().unwrap();
        let after = iter.next().unwrap();
        iter.next();

        let before: Vec<usize> = before[9..19].split(", ").map(|s| s.parse().unwrap()).collect();
        let instr: Vec<usize> = instr.split(" ").map(|s| s.parse().unwrap()).collect();
        let after: Vec<usize> = after[9..19].split(", ").map(|s| s.parse().unwrap()).collect();

        samples.push(Sample {
            in_regs: [before[0], before[1], before[2], before[3]],
            opcode: instr[0] as u8,
            args: [instr[1], instr[2], instr[3]],
            out_regs: [after[0], after[1], after[2], after[3]]
        });
    }
    samples
}


fn opcode_translation() -> HashMap<u8, u8> {
    let mut candidates: HashMap<u8, HashSet<u8>> = HashMap::new();
    for sample in parse_samples() {
        let matches = matching_opcodes(&sample)
            .into_iter()
            .collect::<HashSet<u8>>();

        match candidates.entry(sample.opcode) {
            Entry::Vacant(entry) => {
                entry.insert(matches);
            },
            Entry::Occupied(mut entry) => {
                *entry.get_mut() = matches.intersection(entry.get()).cloned().collect();
            }
        }
    }

    while candidates.values().any(|s| s.len() > 1) {
        let identified_opcodes: HashSet<u8> = candidates
            .values()
            .filter(|s| s.len() == 1)
            .flatten()
            .cloned()
            .collect();
        for (_, v) in candidates.iter_mut().filter(|(_,v)| v.len() > 1) {
            *v = v.difference(&identified_opcodes).cloned().collect();
        }
    }

    candidates
        .into_iter()
        .map(|(k,v)| (k,*v.iter().next().unwrap()))
        .collect()
}


fn part_one() -> usize {
    parse_samples()
        .iter()
        .map(matching_opcodes)
        .filter(|matches| matches.len() >= 3)
        .count()
}


fn part_two() -> usize {
    let input = fs::read_to_string("input-part2.txt").unwrap();
    let translation = opcode_translation();
    let mut regs = [0,0,0,0];
    for line in input.lines() {
        let instr: Vec<usize> = line
            .split(" ")
            .map(|s| s.parse().unwrap())
            .collect();
        exec(translation[&(instr[0] as u8)], &[instr[1],instr[2],instr[3]], &mut regs);
    }

    regs[0]
}


fn main() {
    println!("Part one: {}", part_one());
    println!("Part two: {}", part_two());
}

const PUZZLE_INPUT: usize = 286051;


fn combine(scores: &mut Vec<u8>, idx_0: &mut usize, idx_1: &mut usize) {
    let score_0 = scores[*idx_0] as usize;
    let score_1 = scores[*idx_1] as usize;
    let sum = score_0 + score_1;

    if sum < 10 {
        scores.push(sum as u8);
    } else {
        scores.push((sum / 10) as u8);
        scores.push((sum % 10) as u8);
    }

    *idx_0 = (*idx_0 + score_0 + 1) % scores.len();
    *idx_1 = (*idx_1 + score_1 + 1) % scores.len();
}


fn part_one() -> Vec<u8> {
    let mut scores = vec![3, 7];
    let mut idx_0 = 0;
    let mut idx_1 = 1;

    for _ in 2 .. PUZZLE_INPUT + 10 {
        combine(&mut scores, &mut idx_0, &mut idx_1);
    }

    scores[PUZZLE_INPUT..PUZZLE_INPUT + 10].to_vec()
}


fn part_two() -> usize {
    let mut scores = vec![3, 7];
    let mut idx_0 = 0;
    let mut idx_1 = 1;

    let needle = PUZZLE_INPUT
        .to_string()
        .chars()
        .map(|c| c as u8 - '0' as u8)
        .collect::<Vec<u8>>();

    while !scores.ends_with(&needle) &&
          !scores[..scores.len() - 1].ends_with(&needle) {
        combine(&mut scores, &mut idx_0, &mut idx_1);
    }

    if scores.ends_with(&needle) {
        scores.len() - needle.len()
    } else {
        scores.len() - needle.len() - 1
    }
}


fn main() {
    println!("Part one: {:?}", part_one());
    println!("Part two: {:?}", part_two());
}


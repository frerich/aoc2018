use std::fs;


fn walk(s: &[char], pos: &mut usize) -> String {
    let mut result = String::new();

    while *pos < s.len() {
        match s[*pos] {
            'E' => result.push('E'),
            'W' => result.push('W'),
            'N' => result.push('N'),
            'S' => result.push('S'),
            '(' => {
                let mut longest_route = String::new();

                while s[*pos] != ')' {
                    *pos += 1;

                    let route = walk(s, pos);
                    if route.len() > longest_route.len() {
                        longest_route = route;
                    }
                }

                result.push_str(&longest_route);
            },
            '|' => break,
            ')' => break,
            _   => {}
        }
        *pos += 1;
    }

    result
}


fn longest_route(s: &str) -> String {
    let chars = s.chars().collect::<Vec<char>>();
    walk(&chars, &mut 0)
}


fn eliminate_loops(s: &str) -> String {
    let mut visited: Vec<(i32, i32)> = Vec::new();
    let mut pos: (i32, i32) = (0,0);
    let mut steps = String::new();

    visited.push(pos);

    for c in s.chars() {
        pos = match c {
            'N' => (pos.0    , pos.1 - 1),
            'S' => (pos.0    , pos.1 + 1),
            'W' => (pos.0 - 1, pos.1    ),
            'E' => (pos.0 + 1, pos.1    ),
            _   => pos
        };
        if visited.contains(&pos) {
            while visited[visited.len() - 1] != pos {
                visited.pop();
                steps.pop();
            }
        } else {
            steps.push(c);
            visited.push(pos);
        }
    }

    steps
}


fn part_one(s: &str) -> usize {
    eliminate_loops(&longest_route(s)).len()
}


fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    println!("Part one: {}", part_one(&input));
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty() {
        let input = "^$";
        assert_eq!(longest_route(input), "");
        assert_eq!(part_one(input), 0);
    }

    #[test]
    fn initial_example() {
        let input = "^WNE$";
        assert_eq!(longest_route(input), "WNE");
        assert_eq!(part_one(input), 3);
    }

    #[test]
    fn simple_branch() {
        let input = "^N(E|W)N$";
        assert_eq!(longest_route(input), "NEN");
        assert_eq!(part_one(input), 3);
    }

    #[test]
    fn simple_branch_with_empty_option() {
        let input = "^N(E|W|)N$";
        assert_eq!(longest_route(input), "NEN");
        assert_eq!(part_one(input), 3);
    }

    #[test]
    fn multiple_branches() {
        let input = "^ENWWW(NEEE|SSE(EE|N))$";
        assert_eq!(longest_route(input), "ENWWWSSEEE");
        assert_eq!(part_one(input), 10);
    }

    #[test]
    fn empty_options() {
        let input = "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$";
        assert_eq!(longest_route(input), "ENNWSWWNEWSSSSEENWNSEEESWENNNN");
        assert_eq!(part_one(input), 18);
    }

    #[test]
    fn extra_example_1() {
        let input = "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$";
        assert_eq!(longest_route(input), "ESSWWNNNENNWWWSSSSENNNE");
        assert_eq!(part_one(input), 23);
    }

    #[test]
    fn extra_example_2() {
        let input = "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$";
        assert_eq!(longest_route(input), "WSSEESWWWNWNENNEEEENNESSSSWNWSW");
        assert_eq!(part_one(input), 31);
    }
}


use std::fs::File;
use std::io::Read;


#[derive(Debug)]
struct Node {
    metadata: Vec<i32>,
    children: Vec<Node>
}


fn main() {
    let mut input_file = File::open("input.txt").unwrap();

    let mut contents = String::new();
    input_file.read_to_string(&mut contents);

    let numbers: Vec<i32> = contents.split_whitespace().map(|s| s.parse().unwrap()).collect();

    let (_, tree) = parse(&numbers);

    println!("Part one: {}", part_one(&tree));
    println!("Part two: {}", part_two(&tree));
}


fn part_one(node: &Node) -> i32 {
    node.metadata.iter().sum::<i32>() + node.children.iter().map(part_one).sum::<i32>()
}


fn part_two(node: &Node) -> i32 {
    if node.children.is_empty() {
        node.metadata.iter().sum()
    } else {
        node.metadata
            .iter()
            .map(|d| if let Some(child) = node.children.get(*d as usize - 1) { part_two(child) } else { 0 })
            .sum()
    }
}


fn parse(numbers: &[i32]) -> (&[i32], Node) {
    let num_children = numbers[0];
    let num_metadata = numbers[1];

    let mut data = &numbers[2..];

    let mut children: Vec<Node> = Vec::new();
    for _ in 0 .. num_children {
        let (next_data, node) = parse(data);
        children.push(node);
        data = next_data;
    }

    let metadata: Vec<i32> = data[..num_metadata as usize].to_vec();

    (&data[num_metadata as usize..], Node { metadata, children })
}

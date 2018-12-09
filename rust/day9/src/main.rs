use std::collections::LinkedList;


// Rotate a linked list to the left; 'distance' must be within [0..l.len())
fn rotate<T>(l: & mut LinkedList<T>, distance: usize)
{
    let mut tail: LinkedList<T> = l.split_off(distance);
    tail.append(l);
    *l = tail;
}


fn play(num_players: usize, num_marbles: u32) -> u32 {
    let mut scores: Vec<u32> = vec![0; num_players];

    let mut circle: LinkedList<u32> = LinkedList::new();
    circle.push_back(0);

    for (player, marble) in (1..=num_players).cycle().zip(1..=num_marbles) {
        if false {
            println!("[{}] {:?}", player, circle);
        }

        if marble % 23 == 0 {
            let len = circle.len();
            rotate(&mut circle, len - 7);
            scores[player as usize - 1] += marble + circle.pop_front().unwrap();
        } else if marble == 1 { // Ugly special case. :-/
            circle.push_front(marble);
        } else {
            rotate(&mut circle, 2);
            circle.push_front(marble);
        }
    }

    scores.into_iter().max().unwrap()
}


fn main() {
    println!("Part one: {}", play(411, 72059));
    println!("Part two: {}", play(411, 72059 * 100));
}

use std::fs;
use std::collections::HashSet;


#[derive(Debug)]
struct Command {
    opcode: String,
    args: [usize; 3]
}


#[derive(Debug)]
struct Program {
    ip_register: usize,
    code: Vec<Command>
}


#[derive(Debug)]
struct Machine {
    ip: usize,
    regs: [usize; 6]
}


fn exec(cmd: &Command, m: &mut Machine, ip_reg: usize) {
    let [a, b, c] = cmd.args;

    m.regs[ip_reg] = m.ip;
    // print!("{} {:?} {} {} {} {}", m.ip, m.regs, cmd.opcode, cmd.args[0], cmd.args[1], cmd.args[2]);
    m.regs[c] = match &cmd.opcode[..] {
        "addr" => m.regs[a] + m.regs[b],
        "addi" => m.regs[a] + b,
        "mulr" => m.regs[a] * m.regs[b],
        "muli" => m.regs[a] * b,
        "banr" => m.regs[a] & m.regs[b],
        "bani" => m.regs[a] & b,
        "borr" => m.regs[a] | m.regs[b],
        "bori" => m.regs[a] | b,
        "setr" => m.regs[a],
        "seti" => a,
        "gtir" => if a > m.regs[b] { 1 } else { 0 },
        "gtri" => if m.regs[a] > b { 1 } else { 0 },
        "gtrr" => if m.regs[a] > m.regs[b] { 1 } else { 0 },
        "eqir" => if a == m.regs[b] { 1 } else { 0 },
        "eqri" => if m.regs[a] == b { 1 } else { 0 },
        "eqrr" => if m.regs[a] == m.regs[b] { 1 } else { 0 },
        _ => 0
    };
    // println!(" {:?}", m.regs);
    m.ip = m.regs[ip_reg];
    m.ip += 1;
}


fn parse(input: &str) -> Program {
    let mut lines = input.lines();

    let ip_register: usize = lines.next().unwrap()[4..].parse().unwrap();
    let mut code = Vec::new();
    while let Some(line) = lines.next() {
        let mut tokens = line.split(" ");
        let cmd = tokens.next().unwrap();
        let arg_a = tokens.next().unwrap().parse().unwrap();
        let arg_b = tokens.next().unwrap().parse().unwrap();
        let arg_c = tokens.next().unwrap().parse().unwrap();
        code.push(Command { opcode: cmd.to_string(), args: [arg_a, arg_b, arg_c] });
    }

    Program { ip_register, code }
}


fn part_one(p: &Program) -> usize {
    let mut m = Machine { ip: 0, regs: [0,0,0,0,0,0] };

    while let Some(cmd) = p.code.get(m.ip) {
        exec(cmd, &mut m, p.ip_register);

        // Instruction 28 is the conditional of the outermost loop at which
        // register 0 is compared with register 4. So let's print the value
        // of this register at that point to determine what value register 0
        // has to assume in order to leave the loop.
        if m.ip == 28 {
            return m.regs[4];
        }
    }

    0 // not reachable
}


fn part_two(p: &Program) -> usize {
    let mut m = Machine { ip: 0, regs: [0,0,0,0,0,0] };

    // Keep track of the seen values for register 4 (which tells whether the outermost loop
    // finishes)
    let mut last_reg4_val: usize = 0;
    let mut seen_reg4_vals: HashSet<usize> = HashSet::new();
    seen_reg4_vals.insert(m.regs[4]);

    while let Some(cmd) = p.code.get(m.ip) {
        exec(cmd, &mut m, p.ip_register);

        if m.ip == 28 {
            // If we saw the current value for register 4 already, we are about to enter a loop; if
            // so, return the value we saw on the previous iteration.
            if seen_reg4_vals.contains(&m.regs[4]) {
                return last_reg4_val;
            }
            last_reg4_val = m.regs[4];
            seen_reg4_vals.insert(last_reg4_val);
        }
    }

    0 // not reachable
}


fn main() {
    let input = fs::read_to_string("input.txt").unwrap();

    let program = parse(&input);

    println!("Part one: {}", part_one(&program));
    println!("Part two: {}", part_two(&program));
}

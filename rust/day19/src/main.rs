use std::fs;


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
    }

    m.regs[0]
}


fn part_two(p: &Program) -> usize {
    let mut m = Machine { ip: 0, regs: [1,0,0,0,0,0] };

    // Run until instruction at address 35: at this point, register 2 holds
    // the value for which we want to compute the sum of all divisors.
    while let Some(cmd) = p.code.get(m.ip) {
        exec(cmd, &mut m, p.ip_register);
        if m.ip == 35 {
            break;
        }
    }

    (1..m.regs[2] + 1).filter(|d| m.regs[2] % d == 0).sum()
}


fn main() {
    let input = fs::read_to_string("input.txt").unwrap();

    let program = parse(&input);

    println!("Part one: {}", part_one(&program));
    println!("Part two: {}", part_two(&program));
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sample() {
        let input =
            "#ip 0\n\
             seti 5 0 1\n\
             seti 6 0 2\n\
             addi 0 1 0\n\
             addr 1 2 3\n\
             setr 1 0 0\n\
             seti 8 0 4\n\
             seti 9 0 5";
        assert_eq!(part_one(&parse(input)), 6);
    }
}


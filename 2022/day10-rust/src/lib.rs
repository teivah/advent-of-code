extern crate core;

pub fn signal1(input: &str) -> i32 {
    let mut current_instruction = None;
    let mut cycle = 0;
    let mut cycle_left = 0;
    let mut res = 0;
    let mut reg_x = 1;

    input.lines().for_each(|ins| {
        if ins == "noop" {
            cycle_left = 0;
            current_instruction = None;
        } else if ins.starts_with("addx") {
            let add: i32 = ins[5..].parse().unwrap();
            current_instruction = Some(Add { add });
            cycle_left = 1;
        }

        cycle += 1;
        res += increase_res(cycle, reg_x);

        while cycle_left > 0 {
            cycle_left -= 1;
            cycle += 1;
            res += increase_res(cycle, reg_x);

            match &current_instruction {
                None => (),
                Some(a) => reg_x += a.add,
            };
        }
    });

    res
}

fn increase_res(cycle: i32, reg_x: i32) -> i32 {
    if (cycle - 20) % 40 == 0 {
        return reg_x * cycle;
    }
    return 0;
}

pub fn signal2(input: &str) {
    let mut crt = CRT::new(6, 40);

    let mut current_instruction = None;
    let mut cycle = 0;
    let mut cycle_left = 0;
    let mut reg_x = 1;

    input.lines().for_each(|ins| {
        if ins == "noop" {
            cycle_left = 0;
            current_instruction = None;
        } else if ins.starts_with("addx") {
            let add: i32 = ins[5..].parse().unwrap();
            current_instruction = Some(Add { add });
            cycle_left = 1;
        }

        crt.update(cycle, reg_x);
        cycle += 1;

        while cycle_left > 0 {
            cycle_left -= 1;
            crt.update(cycle, reg_x);
            cycle += 1;

            match &current_instruction {
                None => (),
                Some(a) => reg_x += a.add,
            };
        }
    });

    crt.print();
}

struct CRT {
    lines: Vec<Vec<char>>,
    row: usize,
    col: usize,
}

impl CRT {
    fn new(row: usize, col: usize) -> Self {
        let mut lines = Vec::new();
        for _ in 0..row {
            let mut line = Vec::new();
            for _ in 0..col {
                line.push('.');
            }
            lines.push(line);
        }

        CRT { lines, row, col }
    }

    fn print(&self) {
        self.lines.iter().for_each(|line| {
            line.iter().for_each(|c| print!("{}", c));
            println!();
        })
    }

    fn update(&mut self, cycle: i32, reg_x: i32) {
        let pos = cycle % 40;
        if pos == reg_x || pos - 1 == reg_x || pos + 1 == reg_x {
            let idx = (cycle / 40) as usize;

            self.lines[idx][pos as usize] = '#';
        }
    }
}

struct Add {
    add: i32,
}

struct Noop {}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn test_signal1_unit() {
        let s = fs::read_to_string("test.txt").unwrap();
        assert_eq!(signal1(s.as_str()), 13140);
    }

    #[test]
    fn test_signal1_test() {
        let s = fs::read_to_string("input.txt").unwrap();
        assert_eq!(signal1(s.as_str()), 5245);
    }

    #[test]
    fn test_signal2_unit() {
        let s = fs::read_to_string("test.txt").unwrap();
        signal2(s.as_str())
    }

    #[test]
    fn test_signal2_test() {
        let s = fs::read_to_string("input.txt").unwrap();
        signal2(s.as_str())
    }
}

extern crate core;

use std::any::Any;

pub fn signal1(input: &str) -> i32 {
    let instructions: Vec<_> = input.lines().collect();

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
            current_instruction = Some(Add {
                cycles_left: 1,
                add,
            });
            cycle_left = 1;
        }

        cycle += 1;
        res += increase_res(cycle, reg_x);

        while cycle_left > 0 {
            cycle_left -= 1;
            cycle += 1;

            match &current_instruction {
                None => (),
                Some(a) => res += a.add,
            };
            res += increase_res(cycle, reg_x);
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

struct Add {
    cycles_left: i32,
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
    fn test_signal1_input() {
        let s = fs::read_to_string("input.txt").unwrap();
        assert_eq!(signal1(s.as_str()), 5245);
    }
}

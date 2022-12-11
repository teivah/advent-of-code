extern crate core;

use std::collections::{HashMap, HashSet};

pub fn monkey1(input: &str) -> i32 {
    let lines: Vec<_> = input.lines().collect();
    let mut iter = lines.iter();

    let mut item_count = 0;
    let mut monkeys = Vec::new();
    loop {
        // Monkey x
        let first = iter.next();
        match first {
            None => break,
            _ => (),
        }

        // Starting items
        let s = &iter.next().unwrap()[19..];
        let items = s
            .split(", ")
            .into_iter()
            .map(|s| s.parse::<i32>().unwrap())
            .collect::<Vec<_>>();

        // Operation
        let s = iter.next().unwrap();
        let operator = &s[24..25];
        let operand = &s[26..];
        let operation = |old: i32| -> i32 {
            if operand == "old" {
                match operator {
                    "*" => old * old,
                    "+" => old + old,
                    _ => panic!("unknown operator"),
                }
            } else {
                let v = operand.parse::<i32>().unwrap();
                match operator {
                    "*" => old * v,
                    "+" => old + v,
                    _ => panic!("unknown operator"),
                }
            }
        };

        // Divisible
        let s = iter.next().unwrap();
        let divisible = s[22..].parse::<i32>().unwrap();

        // If true
        let s = iter.next().unwrap();
        let if_true = s[30..].parse::<i32>().unwrap();

        // If false
        let s = iter.next().unwrap();
        let if_false = s[31..].parse::<i32>().unwrap();

        monkeys.push(Monkey {
            divisible,
            if_true,
            if_false,
            operation,
        });

        item_count += 1;
    }

    // let worry_level = HashMap::new();

    1
}

struct Monkey<'a> {
    divisible: i32,
    if_true: i32,
    if_false: i32,
    operation: F,
    operator: &'a str,
    operand: &'a str,
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn test_monkey1_unit() {
        let s = fs::read_to_string("test.txt").unwrap();
        assert_eq!(monkey1(s.as_str()), 10605);
    }

    #[test]
    fn test_rope1_input() {
        let s = fs::read_to_string("input.txt").unwrap();
        assert_eq!(monkey1(s.as_str()), 5245);
    }
}

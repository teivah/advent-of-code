extern crate core;

use crate::ActionType::{Toggle, TurnOff, TurnOn};

pub fn fn1(input: &str) -> i32 {
    let lines: Vec<_> = input.lines().collect();

    let actions = lines
        .iter()
        .map(|s| {
            let action;
            let start;
            match s.chars().nth(6).unwrap() {
                'f' => {
                    action = TurnOff;
                    start = 9;
                }
                'n' => {
                    action = TurnOn;
                    start = 8;
                }
                ' ' => {
                    action = Toggle;
                    start = 7;
                }
                _ => panic!("unhandled"),
            }

            let end = s[start + 1..].find(' ').unwrap() + start + 1;
            let from = Pos::new(&s[start..end]);

            let th = s.find("through ").unwrap() + "through ".len();
            let to = Pos::new(&s[th..]);

            Action {
                action_type: action,
                from,
                to,
            }
        })
        .collect::<Vec<_>>();

    let mut grid = Vec::with_capacity(1000);
    for _ in 0..1000 {
        grid.push(vec![false; 1000]);
    }

    for action in actions {
        for row in action.from.row..=action.to.row {
            for col in action.from.col..=action.to.col {
                match action.action_type {
                    TurnOn => grid[row][col] = true,
                    TurnOff => grid[row][col] = false,
                    Toggle => grid[row][col] = !grid[row][col],
                }
            }
        }
    }

    grid.iter()
        .map(|row| row.iter().filter(|&&v| v).count())
        .fold(0, |sum, i| sum + i as i32)
}

#[derive(Debug)]
enum ActionType {
    TurnOn,
    TurnOff,
    Toggle,
}

struct Action {
    action_type: ActionType,
    from: Pos,
    to: Pos,
}

#[derive(Debug)]
struct Pos {
    row: usize,
    col: usize,
}

impl Pos {
    fn new(s: &str) -> Self {
        let i = s.find(',').unwrap();
        Pos {
            row: s[..i].parse::<usize>().unwrap(),
            col: s[i + 1..].parse::<usize>().unwrap(),
        }
    }
}

pub fn fn2(input: &str) -> i32 {
    let lines: Vec<_> = input.lines().collect();

    let actions = lines
        .iter()
        .map(|s| {
            let action;
            let start;
            match s.chars().nth(6).unwrap() {
                'f' => {
                    action = TurnOff;
                    start = 9;
                }
                'n' => {
                    action = TurnOn;
                    start = 8;
                }
                ' ' => {
                    action = Toggle;
                    start = 7;
                }
                _ => panic!("unhandled"),
            }

            let end = s[start + 1..].find(' ').unwrap() + start + 1;
            let from = Pos::new(&s[start..end]);

            let th = s.find("through ").unwrap() + "through ".len();
            let to = Pos::new(&s[th..]);

            Action {
                action_type: action,
                from,
                to,
            }
        })
        .collect::<Vec<_>>();

    let mut grid = Vec::with_capacity(1000);
    for _ in 0..1000 {
        grid.push(vec![0; 1000]);
    }

    for action in actions {
        for row in action.from.row..=action.to.row {
            for col in action.from.col..=action.to.col {
                match action.action_type {
                    TurnOn => grid[row][col] += 1,
                    TurnOff => {
                        grid[row][col] -= 1;
                        if grid[row][col] < 0 {
                            grid[row][col] = 0;
                        }
                    }
                    Toggle => grid[row][col] += 2,
                }
            }
        }
    }

    let mut sum = 0;
    for row in 0..1000 {
        for col in 0..1000 {
            sum += grid[row][col];
        }
    }
    sum
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn test_fn1_input() {
        let s = fs::read_to_string("input.txt").unwrap();
        assert_eq!(fn1(s.as_str()), 1);
    }

    #[test]
    fn test_fn2_input() {
        let s = fs::read_to_string("input.txt").unwrap();
        assert_eq!(fn2(s.as_str()), 1);
    }
}

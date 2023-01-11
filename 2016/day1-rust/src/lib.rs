extern crate core;

use crate::Heading::{East, North, South, West};
use std::collections::HashSet;

pub fn fn1(s: &str) -> i32 {
    let indices = index_all(s, ", ");

    let mut instructions = Vec::new();
    let mut previous = 0;
    for idx in indices.iter() {
        instructions.push(Instruction::new(&s[previous..*idx]));
        previous = *idx + 2
    }

    let last = &s[indices[indices.len() - 1] + 2..];
    instructions.push(Instruction::new(last));

    let mut x = 0;
    let mut y = 0;
    let mut heading = North;
    for instruction in instructions.iter() {
        heading = heading.apply_turn(&instruction.direction);
        (x, y) = heading.apply_move(x, y, instruction.len)
    }

    x.abs() + y.abs()
}

#[derive(Debug)]
enum Direction {
    Right,
    Left,
}

enum Heading {
    North,
    South,
    East,
    West,
}

impl Heading {
    fn apply_turn(&self, d: &Direction) -> Heading {
        match self {
            North => match d {
                Direction::Right => East,
                Direction::Left => West,
            },
            South => match d {
                Direction::Right => West,
                Direction::Left => East,
            },
            East => match d {
                Direction::Right => South,
                Direction::Left => North,
            },
            West => match d {
                Direction::Right => North,
                Direction::Left => South,
            },
        }
    }

    fn apply_move(&self, x: i32, y: i32, n: i32) -> (i32, i32) {
        match self {
            North => (x, y + n),
            South => (x, y - n),
            East => (x + n, y),
            West => (x - n, y),
        }
    }

    fn is_visited(
        &self,
        visited: &mut HashSet<(i32, i32)>,
        x: i32,
        y: i32,
        n: i32,
    ) -> Option<(i32, i32)> {
        match self {
            North => {
                for i in 1..=n {
                    if visited.contains(&(x, y + i)) {
                        return Some((x, y + i));
                    }
                    visited.insert((x, y + i));
                }
            }
            South => {
                for i in 1..=n {
                    if visited.contains(&(x, y - i)) {
                        return Some((x, y - i));
                    }
                    visited.insert((x, y - i));
                }
            }
            East => {
                for i in 1..=n {
                    if visited.contains(&(x + i, y)) {
                        return Some((x + i, y));
                    }
                    visited.insert((x + i, y));
                }
            }
            West => {
                for i in 1..=n {
                    if visited.contains(&(x - i, y)) {
                        return Some((x - i, y));
                    }
                    visited.insert((x - i, y));
                }
            }
        }

        None
    }
}

#[derive(Debug)]
struct Instruction {
    direction: Direction,
    len: i32,
}

impl Instruction {
    fn new(s: &str) -> Self {
        let v = s[1..].parse::<i32>().unwrap();

        if s.chars().nth(0).unwrap() == 'R' {
            return Instruction {
                direction: Direction::Right,
                len: v,
            };
        }
        Instruction {
            direction: Direction::Left,
            len: v,
        }
    }
}

pub fn fn2(s: &str) -> i32 {
    let indices = index_all(s, ", ");

    let mut instructions = Vec::new();
    let mut previous = 0;
    for idx in indices.iter() {
        instructions.push(Instruction::new(&s[previous..*idx]));
        previous = *idx + 2
    }

    let last = &s[indices[indices.len() - 1] + 2..];
    instructions.push(Instruction::new(last));

    let mut visited: HashSet<(i32, i32)> = HashSet::new();
    let mut x = 0;
    let mut y = 0;
    let mut heading = North;
    for instruction in instructions.iter() {
        heading = heading.apply_turn(&instruction.direction);
        if let Some((found_x, found_y)) = heading.is_visited(&mut visited, x, y, instruction.len) {
            return found_x.abs() + found_y.abs();
        }
        (x, y) = heading.apply_move(x, y, instruction.len);
    }

    -1
}

fn index_all(s: &str, search: &str) -> Vec<usize> {
    let mut res = Vec::new();
    let mut i = 0;
    while i < s.len() {
        let idx = s[i..].find(search);
        match idx {
            None => return res,
            Some(t) => {
                res.push(t + i);
                i += t + search.len()
            }
        }
    }
    res
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn test_fn1_unit() {
        assert_eq!(fn1("R2, L3"), 5);
        assert_eq!(fn1("R2, R2, R2"), 2);
        assert_eq!(fn1("R5, L5, R5, R3"), 12);
    }

    #[test]
    fn test_fn1_input() {
        let s = fs::read_to_string("input.txt").unwrap();
        assert_eq!(fn1(s.as_str()), 300);
    }

    #[test]
    fn test_fn2_unit() {
        assert_eq!(fn2("R8, R4, R4, R8"), 4);
    }

    #[test]
    fn test_fn2_input() {
        let s = fs::read_to_string("input.txt").unwrap();
        assert_eq!(fn2(s.as_str()), 159);
    }
}

extern crate core;

use crate::Direction::{Down, Left, Right, Up};
use std::collections::HashMap;
use std::hash::Hash;

pub fn fn1(target: i64) -> i64 {
    let mut row = 0;
    let mut col = 0;
    let mut direction = Right;
    let mut max = 1;

    if target == 1 {
        return 0;
    }

    for i in 2..=target {
        match direction {
            Up => {
                row -= 1;
                if row == -max {
                    direction = direction.next();
                }
            }
            Down => {
                row += 1;
                if row == max {
                    direction = direction.next();
                    max += 1;
                }
            }
            Left => {
                col -= 1;
                if col == -max {
                    direction = direction.next();
                }
            }
            Right => {
                col += 1;
                if col == max {
                    direction = direction.next();
                }
            }
        }
    }

    distance(row, col)
}

fn distance(row: i64, col: i64) -> i64 {
    return (row - 0).abs() + (col - 0).abs();
}

#[derive(Debug)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

impl Direction {
    fn next(&self) -> Direction {
        match self {
            Up => Left,
            Down => Right,
            Left => Down,
            Right => Up,
        }
    }
}

pub fn fn2(target: i64) -> i64 {
    let mut row: i64 = 0;
    let mut col: i64 = 0;
    let mut direction = Right;
    let mut max = 1;

    if target == 1 {
        return 0;
    }

    let mut grid: HashMap<(i64, i64), i64> = HashMap::new();

    grid.insert((0, 0), 1);

    for i in 2..=target {
        match direction {
            Up => {
                row -= 1;
                if row == -max {
                    direction = direction.next();
                }
            }
            Down => {
                row += 1;
                if row == max {
                    direction = direction.next();
                    max += 1;
                }
            }
            Left => {
                col -= 1;
                if col == -max {
                    direction = direction.next();
                }
            }
            Right => {
                col += 1;
                if col == max {
                    direction = direction.next();
                }
            }
        }

        let mut sum = 0;
        if let Some(t) = grid.get(&(row - 1, col)) {
            sum += t;
        }
        if let Some(t) = grid.get(&(row - 1, col - 1)) {
            sum += t;
        }
        if let Some(t) = grid.get(&(row, col - 1)) {
            sum += t;
        }
        if let Some(t) = grid.get(&(row + 1, col - 1)) {
            sum += t;
        }
        if let Some(t) = grid.get(&(row + 1, col)) {
            sum += t;
        }
        if let Some(t) = grid.get(&(row + 1, col + 1)) {
            sum += t;
        }
        if let Some(t) = grid.get(&(row, col + 1)) {
            sum += t;
        }
        if let Some(t) = grid.get(&(row - 1, col + 1)) {
            sum += t;
        }
        if sum > target {
            return sum;
        }
        grid.insert((row, col), sum);
    }

    -1
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn test_fn1_unit() {
        assert_eq!(fn1(1), 0);
        assert_eq!(fn1(12), 3);
        assert_eq!(fn1(23), 2);
        assert_eq!(fn1(1024), 31);
    }

    #[test]
    fn test_fn1_input() {
        let s = fs::read_to_string("input.txt").unwrap();
        assert_eq!(fn1(361527), 326);
    }

    #[test]
    fn test_fn2_input() {
        let s = fs::read_to_string("input.txt").unwrap();
        assert_eq!(fn2(361527), 363010);
    }
}

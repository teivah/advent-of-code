extern crate core;

use std::collections::HashMap;

pub fn fn1(input: &str) -> i32 {
    let lines: Vec<_> = input.lines().collect();

    let mut first: i32 = 0;
    let mut last: i32 = 0;
    let mut sum: i32 = 0;
    lines.iter().for_each(|line| {
        for ch in line.chars() {
            if let Some(digit) = ch.to_digit(10) {
                first = digit as i32;
                break;
            }
        }
        for ch in line.chars().rev() {
            if let Some(digit) = ch.to_digit(10) {
                last = digit as i32;
                break;
            }
        }
        sum += first * 10 + last;
    });

    sum
}

pub fn fn2(input: &str) -> i32 {
    let lines: Vec<_> = input.lines().collect();

    let mut sum: i32 = 0;
    let digits: HashMap<_, _> =
        [
            ("one", 1),
            ("two", 2),
            ("three", 3),
            ("four", 4),
            ("five", 5),
            ("six", 6),
            ("seven", 7),
            ("eight", 8),
            ("nine", 9),
        ]
            .iter().cloned().collect();
    lines.iter().for_each(|line| {
        let mut first: i32 = 0;
        let mut first_idx: i32 = std::i32::MAX;
        let mut last: i32 = 0;
        let mut last_idx: i32 = -1;
        digits.iter().for_each(|(key, value)| {
            if let Some(index) = line.find(key) {
                if (index as i32) < first_idx {
                    first = value.clone();
                    first_idx = index as i32;
                }
            }
            if let Some(index) = line.rfind(key) {
                if (index as i32) > last_idx {
                    last = value.clone();
                    last_idx = index as i32;
                }
            }
        });
        for (index, ch) in line.char_indices() {
            if let Some(digit) = ch.to_digit(10) {
                if (index as i32) < first_idx {
                    first = digit as i32;
                }
                break;
            }
        }
        for (index, ch) in line.char_indices().rev() {
            if let Some(digit) = ch.to_digit(10) {
                if (index as i32) > last_idx {
                    last = digit as i32;
                }
                break;
            }
        }

        sum += first * 10 + last;
    });

    sum
}

#[cfg(test)]
mod tests {
    use std::fs;

    use super::*;

    #[test]
    fn test_fn1_unit() {
        let s = fs::read_to_string("test.txt").unwrap();
        assert_eq!(fn1(s.as_str()), 142);
    }

    #[test]
    fn test_fn1_input() {
        let s = fs::read_to_string("input.txt").unwrap();
        assert_eq!(fn1(s.as_str()), 54338);
    }

    #[test]
    fn test_fn2_unit() {
        let s = fs::read_to_string("test2.txt").unwrap();
        assert_eq!(fn2(s.as_str()), 281);
    }

    #[test]
    fn test_fn2_input() {
        let s = fs::read_to_string("input.txt").unwrap();
        assert_eq!(fn2(s.as_str()), 53389);
    }
}

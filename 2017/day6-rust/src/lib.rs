extern crate core;

use std::collections::{HashMap, HashSet};

pub fn fn1(input: &str) -> i32 {
    let lines: Vec<_> = input.lines().collect();
    let mut numbers = parse_numbers(lines[0]);
    let mut seen = HashSet::new();

    let mut steps = 0;
    loop {
        steps += 1;
        numbers = distribute(&numbers);
        let key = format_key(&numbers);
        if seen.contains(&key) {
            break;
        }
        seen.insert(key);
    }

    steps
}

fn distribute(numbers: &Vec<i32>) -> Vec<i32> {
    let mut res = Vec::with_capacity(numbers.len());
    let mut max = 0;
    let mut idx_max = 0;
    for (i, v) in numbers.iter().enumerate() {
        res.push(*v);
        if *v > max {
            max = *v;
            idx_max = i;
        }
    }

    let mut total = max;
    res[idx_max] = 0;
    let mut i = idx_max;
    loop {
        i = (i + 1) % numbers.len();
        res[i] += 1;
        total -= 1;
        if total == 0 {
            break;
        }
    }

    res
}

fn format_key(numbers: &Vec<i32>) -> String {
    numbers.iter().fold(String::new(), |sum, v| {
        sum + &v.to_string().to_string() + "."
    })
}

fn parse_numbers(s: &str) -> Vec<i32> {
    let mut i = 0;
    let mut res = Vec::new();
    while i < s.len() {
        if s.chars().nth(i).unwrap() >= '0' && s.chars().nth(i).unwrap() <= '9' {
            let mut j = i + 1;
            while j < s.len() {
                if s.chars().nth(j).unwrap() >= '0' && s.chars().nth(j).unwrap() <= '9' {
                    j += 1;
                    continue;
                }
                break;
            }
            if j == s.len() {
                res.push(s[i..].parse::<i32>().unwrap());
            } else {
                res.push(s[i..j].parse::<i32>().unwrap());
            }
            i = j + 1;
            continue;
        }

        i += 1
    }
    return res;
}

pub fn fn2(input: &str) -> i32 {
    let lines: Vec<_> = input.lines().collect();
    let mut numbers = parse_numbers(lines[0]);
    let mut seen = HashMap::new();

    let mut steps = 0;
    loop {
        steps += 1;
        numbers = distribute(&numbers);
        let key = format_key(&numbers);
        if let Some(v) = seen.get(&key) {
            return steps - v;
        }
        seen.insert(key, steps);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn test_fn1_unit() {
        let s = fs::read_to_string("test.txt").unwrap();
        assert_eq!(fn1(s.as_str()), 5);
    }

    #[test]
    fn test_fn1_input() {
        let s = fs::read_to_string("input.txt").unwrap();
        assert_eq!(fn1(s.as_str()), 4074);
    }

    #[test]
    fn test_fn2_unit() {
        let s = fs::read_to_string("test.txt").unwrap();
        assert_eq!(fn2(s.as_str()), 4);
    }

    #[test]
    fn test_fn2_input() {
        let s = fs::read_to_string("input.txt").unwrap();
        assert_eq!(fn2(s.as_str()), 2793);
    }
}

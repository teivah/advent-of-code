extern crate core;

use std::collections::{HashMap, HashSet};

pub fn fn1(s: &str) -> i32 {
    let mut visited: HashMap<i32, HashSet<i32>> = HashMap::new();
    visited.entry(0).or_insert(HashSet::new()).insert(0);

    let mut x = 0;
    let mut y = 0;
    let mut sum = 1;

    for c in s.chars() {
        match c {
            '<' => x -= 1,
            '>' => x += 1,
            '^' => y += 1,
            'v' => y -= 1,
            _ => panic!("unknown"),
        }

        let set = visited.entry(x).or_insert(HashSet::new());
        if !set.contains(&y) {
            sum += 1;
            set.insert(y);
        }
    }

    sum
}

pub fn fn2(s: &str) -> i32 {
    let mut visited: HashMap<i32, HashSet<i32>> = HashMap::new();
    visited.entry(0).or_insert(HashSet::new()).insert(0);

    let mut santa_x = 0;
    let mut santa_y = 0;
    let mut robosanta_x = 0;
    let mut robosanta_y = 0;
    let mut sum = 1;

    for (i, c) in s.chars().enumerate() {
        if i % 2 == 0 {
            match c {
                '<' => santa_x -= 1,
                '>' => santa_x += 1,
                '^' => santa_y += 1,
                'v' => santa_y -= 1,
                _ => panic!("unknown"),
            }
            let set = visited.entry(santa_x).or_insert(HashSet::new());
            if !set.contains(&santa_y) {
                sum += 1;
                set.insert(santa_y);
            }
        } else {
            match c {
                '<' => robosanta_x -= 1,
                '>' => robosanta_x += 1,
                '^' => robosanta_y += 1,
                'v' => robosanta_y -= 1,
                _ => panic!("unknown"),
            }
            let set = visited.entry(robosanta_x).or_insert(HashSet::new());
            if !set.contains(&robosanta_y) {
                sum += 1;
                set.insert(robosanta_y);
            }
        }
    }

    sum
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn test_fn1_unit() {
        assert_eq!(fn1(">"), 2);
        assert_eq!(fn1("^>v<"), 4);
        assert_eq!(fn1("^v^v^v^v^v"), 2);
    }

    #[test]
    fn test_fn1_input() {
        let s = fs::read_to_string("input.txt").unwrap();
        assert_eq!(fn1(s.as_str()), 1);
    }

    #[test]
    fn test_fn2_unit() {
        assert_eq!(fn2("^v"), 3);
        assert_eq!(fn2("^>v<"), 3);
        assert_eq!(fn2("^v^v^v^v^v"), 11);
    }

    #[test]
    fn test_fn2_input() {
        let s = fs::read_to_string("input.txt").unwrap();
        assert_eq!(fn2(s.as_str()), 2631);
    }
}

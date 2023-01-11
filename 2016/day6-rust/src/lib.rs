extern crate core;

use std::collections::HashMap;

pub fn fn1(input: &str) -> String {
    let lines: Vec<_> = input.lines().collect();

    let l = lines[0].len();

    let mut res = String::new();
    for i in 0..l {
        let mut count = HashMap::new();
        for s in &lines {
            let x = count.entry(s.chars().nth(i).unwrap()).or_insert(0);
            *x += 1;
        }

        let mut max = 0;
        let mut char = ' ';
        count.iter().for_each(|(c, &v)| {
            if v > max {
                max = v;
                char = *c;
            }
        });
        res.push(char);
    }

    res
}

pub fn fn2(input: &str) -> String {
    let lines: Vec<_> = input.lines().collect();

    let l = lines[0].len();

    let mut res = String::new();
    for i in 0..l {
        let mut count = HashMap::new();
        for s in &lines {
            let x = count.entry(s.chars().nth(i).unwrap()).or_insert(0);
            *x += 1;
        }

        let mut min = std::i32::MAX;
        let mut char = ' ';
        count.iter().for_each(|(c, &v)| {
            if v < min {
                min = v;
                char = *c;
            }
        });
        res.push(char);
    }

    res
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn test_fn1_unit() {
        let s = fs::read_to_string("test.txt").unwrap();
        assert_eq!(fn1(s.as_str()), "easter");
    }

    #[test]
    fn test_fn1_input() {
        let s = fs::read_to_string("input.txt").unwrap();
        assert_eq!(fn1(s.as_str()), "");
    }

    #[test]
    fn test_fn2_unit() {
        let s = fs::read_to_string("test.txt").unwrap();
        assert_eq!(fn2(s.as_str()), "advent");
    }

    #[test]
    fn test_fn2_input() {
        let s = fs::read_to_string("input.txt").unwrap();
        assert_eq!(fn2(s.as_str()), "advent");
    }
}

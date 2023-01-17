extern crate core;

pub fn fn1(input: &str) -> i32 {
    let lines: Vec<_> = input.lines().collect();
    let line = lines[0];
    let mut sum = 0;
    for i in 1..line.len() {
        let a = line.chars().nth(i - 1).unwrap();
        let b = line.chars().nth(i).unwrap();
        if a == b {
            sum += a.to_digit(10).unwrap();
        }
    }

    if line.chars().nth(0).unwrap() == line.chars().nth(line.len() - 1).unwrap() {
        sum += line.chars().nth(0).unwrap().to_digit(10).unwrap();
    }
    sum as i32
}

pub fn fn2(input: &str) -> i32 {
    let lines: Vec<_> = input.lines().collect();
    let line = lines[0];
    let mut sum = 0;
    for i in 0..line.len() {
        let a = line.chars().nth(i).unwrap();
        let i1 = (i + line.len() / 2) % line.len();
        let b = line.chars().nth(i1).unwrap();
        if a == b {
            sum += a.to_digit(10).unwrap();
        }
    }
    sum as i32
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn test_fn1_input() {
        let s = fs::read_to_string("input.txt").unwrap();
        assert_eq!(fn1(s.as_str()), 1150);
    }

    #[test]
    fn test_fn2_input() {
        let s = fs::read_to_string("input.txt").unwrap();
        assert_eq!(fn2(s.as_str()), 10695);
    }
}

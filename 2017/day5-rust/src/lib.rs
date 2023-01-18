extern crate core;

pub fn fn1(input: &str) -> i32 {
    let mut lines: Vec<_> = input.lines().map(|s| s.parse::<i32>().unwrap()).collect();

    let mut i: i32 = 0;
    let mut steps = 0;
    while i < lines.len() as i32 {
        let offset = lines[i as usize];
        steps += 1;
        lines[i as usize] += 1;
        i += offset;
    }

    steps
}

pub fn fn2(input: &str) -> i32 {
    let mut lines: Vec<_> = input.lines().map(|s| s.parse::<i32>().unwrap()).collect();

    let mut i: i32 = 0;
    let mut steps = 0;
    while i < lines.len() as i32 {
        let offset = lines[i as usize];
        steps += 1;
        if offset >= 3 {
            lines[i as usize] -= 1;
        } else {
            lines[i as usize] += 1;
        }
        i += offset;
    }

    steps
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
        assert_eq!(fn1(s.as_str()), 372671);
    }

    #[test]
    fn test_fn2_unit() {
        let s = fs::read_to_string("test.txt").unwrap();
        assert_eq!(fn2(s.as_str()), 10);
    }

    #[test]
    fn test_fn2_input() {
        let s = fs::read_to_string("input.txt").unwrap();
        assert_eq!(fn2(s.as_str()), 25608480);
    }
}

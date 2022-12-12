extern crate core;

pub fn fn1(input: &str) -> i32 {
    let lines: Vec<_> = input.lines().collect();

    1
}

pub fn fn2(input: &str) -> i32 {
    let lines: Vec<_> = input.lines().collect();

    1
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn test_fn1_unit() {
        let s = fs::read_to_string("test.txt").unwrap();
        assert_eq!(fn1(s.as_str()), 1);
    }

    #[test]
    fn test_fn1_input() {
        let s = fs::read_to_string("input.txt").unwrap();
        assert_eq!(fn1(s.as_str()), 1);
    }

    #[test]
    fn test_fn2_unit() {
        let s = fs::read_to_string("test.txt").unwrap();
        assert_eq!(fn2(s.as_str()), 1);
    }

    #[test]
    fn test_fn2_input() {
        let s = fs::read_to_string("input.txt").unwrap();
        assert_eq!(fn2(s.as_str()), 1);
    }
}

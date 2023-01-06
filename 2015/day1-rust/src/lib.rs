extern crate core;

pub fn fn1(s: &str) -> i32 {
    s.chars().map(|c| match c {
       '(' => 1,
        ')' => -1,
        _ => 0,
    }).fold(0, |sum, i| sum + i)
}

pub fn fn2(s: &str) -> i32 {
    let mut n = 0;
    for (i, c) in s.chars().enumerate() {
        match c {
            '(' => n += 1,
            ')' => n-=1,
            _ => (),
        }
        if n == -1 {
            return i as i32+1
        }
    }

    0
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn test_fn1_unit() {
        assert_eq!(fn1("(())"), 0);
        assert_eq!(fn1("()()"), 0);
        assert_eq!(fn1("((("), 3);
        assert_eq!(fn1("(()(()("), 3);
        assert_eq!(fn1("))((((("), 3);
        assert_eq!(fn1("())"), -1);
        assert_eq!(fn1("))("), -1);
        assert_eq!(fn1(")))"), -3);
        assert_eq!(fn1(")())())"), -3);
    }

    #[test]
    fn test_fn1_input() {
        let s = fs::read_to_string("input.txt").unwrap();
        assert_eq!(fn1(s.as_str()), 280);
    }

    #[test]
    fn test_fn2_unit() {
        assert_eq!(fn2(")"), 1);
        assert_eq!(fn2("()())"), 5);
    }

    #[test]
    fn test_fn2_input() {
        let s = fs::read_to_string("input.txt").unwrap();
        assert_eq!(fn2(s.as_str()), 1797);
    }
}

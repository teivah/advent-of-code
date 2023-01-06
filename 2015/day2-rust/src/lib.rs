extern crate core;

pub fn fn1(input: &str) -> i32 {
    input.lines().into_iter().fold(0, |sum, i| sum + feet(i))
}

fn feet(s: &str) -> i32 {
    let one = s.find("x").unwrap();
    let two = s[one + 1..].find("x").unwrap() + one + 1;

    let l = s[..one].parse::<i32>().unwrap();
    let w = s[one + 1..two].parse::<i32>().unwrap();
    let h = s[two + 1..].parse::<i32>().unwrap();

    2 * l * w + 2 * w * h + 2 * h * l + smallest_side(l, w, h)
}

fn smallest_side(l: i32, w: i32, h: i32) -> i32 {
    let mut v = vec![l, w, h];
    v.sort();
    v[0] * v[1]
}

pub fn fn2(input: &str) -> i32 {
    input.lines().into_iter().fold(0, |sum, s| {
        let one = s.find("x").unwrap();
        let two = s[one + 1..].find("x").unwrap() + one + 1;
        let l = s[..one].parse::<i32>().unwrap();
        let w = s[one + 1..two].parse::<i32>().unwrap();
        let h = s[two + 1..].parse::<i32>().unwrap();

        let mut v = vec![l, w, h];
        v.sort();

        let res = 2 * v[0] + 2 * v[1] + l * w * h;

        sum + res
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn test_fn1_unit() {
        assert_eq!(fn1("2x3x4"), 58);
        assert_eq!(fn1("1x1x10"), 43);
    }

    #[test]
    fn test_fn1_input() {
        let s = fs::read_to_string("input.txt").unwrap();
        assert_eq!(fn1(s.as_str()), 1598415);
    }

    #[test]
    fn test_fn2_unit() {
        assert_eq!(fn2("2x3x4"), 34);
        assert_eq!(fn2("1x1x10"), 14);
    }

    #[test]
    fn test_fn2_input() {
        let s = fs::read_to_string("input.txt").unwrap();
        assert_eq!(fn2(s.as_str()), 3812909);
    }
}

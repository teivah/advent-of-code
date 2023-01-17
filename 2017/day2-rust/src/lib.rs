extern crate core;

pub fn fn1(input: &str) -> i32 {
    input
        .lines()
        .map(|s| parse_numbers(s))
        .map(|v| v.iter().max().unwrap() - v.iter().min().unwrap())
        .fold(0, |sum, v| sum + v)
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
    input
        .lines()
        .map(|s| parse_numbers(s))
        .map(|v| {
            let length = v.len();
            for i in 0..length - 1 {
                for j in i + 1..length {
                    let a = v[i];
                    let b = v[j];
                    if a % b == 0 {
                        return (a / b) as i32;
                    }
                    if b % a == 0 {
                        return (b / a) as i32;
                    }
                }
            }
            0
        })
        .fold(0, |sum, v| sum + v)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn test_fn1_unit() {
        let s = fs::read_to_string("test.txt").unwrap();
        assert_eq!(fn1(s.as_str()), 18);
    }

    #[test]
    fn test_fn1_input() {
        let s = fs::read_to_string("input.txt").unwrap();
        assert_eq!(fn1(s.as_str()), 58975);
    }

    #[test]
    fn test_fn2_unit() {
        let s = fs::read_to_string("test2.txt").unwrap();
        assert_eq!(fn2(s.as_str()), 9);
    }

    #[test]
    fn test_fn2_input() {
        let s = fs::read_to_string("input.txt").unwrap();
        assert_eq!(fn2(s.as_str()), 1);
    }
}

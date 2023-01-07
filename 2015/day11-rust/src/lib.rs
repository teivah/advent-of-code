extern crate core;

use std::collections::{HashMap, HashSet};

pub fn fn1(s: &str) -> String {
    let mut pass = s.to_string();
    loop {
        pass = increment(&pass);
        if is_valid(&pass) {
            return pass;
        }
    }
}

fn is_valid(s: &str) -> bool {
    req1(s) && req2(s) && req3(s)
}

fn req1(s: &str) -> bool {
    for i in 0..s.len() - 2 {
        let a = s.chars().nth(i).unwrap();
        let b = s.chars().nth(i + 1).unwrap();
        if next_char(a) == b {
            let c = s.chars().nth(i + 2).unwrap();
            if next_char(b) == c {
                return true;
            }
        }
    }

    false
}

fn req2(s: &str) -> bool {
    for c in s.chars().into_iter() {
        match c {
            'i' | 'o' | 'l' => return false,
            _ => (),
        }
    }

    true
}

fn req3(s: &str) -> bool {
    let mut set = HashSet::new();
    let mut i = 0;
    let mut found_first = false;
    while i < s.len() - 1 {
        let a = s.chars().nth(i).unwrap();
        let b = s.chars().nth(i + 1).unwrap();
        if a == b {
            let key = format!("{}{}", a, b);

            if !set.contains(&key) {
                set.insert(key);
                if found_first {
                    return true;
                }
                found_first = true;
            }
        }
        if i == s.len() - 2 {
            return false;
        }

        if s.chars().nth(i + 2).unwrap() == a {
            i += 2;
        } else {
            i += 1;
        }
    }

    false
}

fn next_char(c: char) -> char {
    std::char::from_u32(c as u32 + 1).unwrap_or(c)
}

fn increment(s: &str) -> String {
    let mut string = s.to_string();
    for i in (0..s.len()).rev() {
        unsafe {
            let mut bytes = string.as_bytes_mut();
            let c = s.chars().nth(i).unwrap();
            if c != 'z' {
                bytes[i] = next_char(c) as u8;
                return string;
            } else {
                bytes[i] = 'a' as u8;
            }
        }
    }

    string
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
    fn test_increment() {
        assert_eq!(increment("a"), "b");
        assert_eq!(increment("xy"), "xz");
        assert_eq!(increment("xz"), "ya");
        assert_eq!(increment("azz"), "baa");
    }

    #[test]
    fn test_fn1_input() {
        assert_eq!(fn1("vzbxkghb"), "vzbxxyzz");
    }

    #[test]
    fn test_fn2_input() {
        assert_eq!(fn1("vzbxxyzz"), "vzcaabcc");
    }
}

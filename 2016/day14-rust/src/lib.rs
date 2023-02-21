extern crate core;

use std::collections::HashSet;

pub fn fn1(salt: &str, nth_key: i64, length: i64) -> i64 {
    let mut chars = Vec::new();
    let mut found = 0;
    let mut buffered: Vec<i64> = Vec::new();
    let mut res = None;
    for i in 0.. {
        buffered.retain(|&v| {
            if i - v > length {
                found += 1;
                if found == nth_key {
                    res = Some(v);
                }
                false
            } else {
                true
            }
        });

        if let Some(v) = res {
            return v;
        }

        let h = hash(salt, i);

        // Get all indices where their five char has been found
        let set = five(&h, &chars);
        if set.len() != 0 {
            chars.retain(|char| {
                if char.idx + length <= i {
                    return false;
                }

                if set.contains(&char.idx) {
                    buffered.push(char.idx as i64);
                    return false;
                }

                true
            });
        }

        if let Some(c) = triplet(&h) {
            chars.push(Char { idx: i, c });
        }
    }

    1
}

#[derive(Debug)]
struct Char {
    idx: i64,
    c: char,
}

fn hash(salt: &str, n: i64) -> String {
    let v = format!("{}{}", salt, n);
    let digest = md5::compute(v);
    return format!("{:x}", digest);
}

fn plain_hash(s: &str) -> String {
    let digest = md5::compute(s);
    return format!("{:x}", digest);
}

fn triplet(s: &str) -> Option<char> {
    for i in 2..s.len() {
        let c = s.chars().nth(i).unwrap();
        if c == s.chars().nth(i - 2).unwrap() && c == s.chars().nth(i - 1).unwrap() {
            return Some(c);
        }
    }

    None
}

fn five(s: &str, chars: &Vec<Char>) -> HashSet<i64> {
    let mut set = HashSet::new();
    for i in 4..s.len() {
        let c = s.chars().nth(i).unwrap();
        if c == s.chars().nth(i - 1).unwrap()
            && c == s.chars().nth(i - 2).unwrap()
            && c == s.chars().nth(i - 3).unwrap()
            && c == s.chars().nth(i - 4).unwrap()
        {
            for char in chars.iter() {
                if char.c == c {
                    set.insert(char.idx);
                }
            }
        }
    }
    set
}

pub fn fn2(salt: &str, nth_key: i64, length: i64, additional_hashings: i64) -> i64 {
    let mut chars = Vec::new();
    let mut found = 0;
    let mut buffered: Vec<i64> = Vec::new();
    let mut res = None;
    for i in 0.. {
        buffered.retain(|&v| {
            if i - v > length {
                found += 1;
                if found == nth_key {
                    res = Some(v);
                }
                false
            } else {
                true
            }
        });

        if let Some(v) = res {
            return v;
        }

        let mut h = hash(salt, i);
        for _ in 0..additional_hashings {
            h = plain_hash(&h);
        }

        // Get all indices where their five char has been found
        let set = five(&h, &chars);
        if set.len() != 0 {
            chars.retain(|char| {
                if char.idx + length <= i {
                    return false;
                }

                if set.contains(&char.idx) {
                    buffered.push(char.idx as i64);
                    return false;
                }

                true
            });
        }

        if let Some(c) = triplet(&h) {
            chars.push(Char { idx: i, c });
        }
    }

}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn test_fn1_unit() {
        assert_eq!(fn1("abc", 64, 1_000), 22728);
    }

    #[test]
    fn test_fn1_input() {
        assert_eq!(fn1("yjdafjpo", 64, 1_000), 25427);
    }

    #[test]
    fn test_fn2_unit() {
        assert_eq!(fn2("abc", 64, 1_000, 2016), 22551);
    }

    #[test]
    fn test_fn2_input() {
        assert_eq!(fn2("yjdafjpo", 64, 1_000, 2016), 22045);
    }
}

extern crate core;

use std::collections::HashSet;

pub fn fn1(s: &str, zeroes: usize) -> String {
    let mut res = String::new();
    let mut found = 0;
    for i in 0.. {
        let h = hash(s, i);
        if starts_with_zeros(&h, zeroes) {
            res.push(h.chars().nth(zeroes).unwrap());
            found += 1;
            if found == 8 {
                return res;
            }
        }
    }

    res
}

fn hash(s: &str, i: i32) -> String {
    let v = format!("{}{}", s, i);
    let digest = md5::compute(v);
    format!("{:x}", digest)
}

fn starts_with_zeros(s: &str, n: usize) -> bool {
    let input = std::iter::repeat("0").take(n).collect::<String>();
    s.starts_with(&input)
}

pub fn fn2(s: &str, zeroes: usize) -> String {
    let mut res = std::iter::repeat("x").take(8).collect::<String>();
    println!("{}", res);
    let mut set: HashSet<u32> = HashSet::new();
    for i in 0.. {
        let h = hash(s, i);
        if starts_with_zeros(&h, zeroes) {
            let sp = h.chars().nth(zeroes).unwrap();
            if let Some(v) = sp.to_digit(10) {
                if v > 7 {
                    continue;
                }
                if set.contains(&v) {
                    continue;
                }

                set.insert(v);
                let c = h.chars().nth(zeroes + 1).unwrap();
                let mut chars = res.chars().collect::<Vec<char>>();
                chars[v as usize] = c;
                res = chars.into_iter().collect::<String>();
                println!("{}", res);
                if set.len() == 8 {
                    return res;
                }
            }
        }
    }

    res
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn test_fn1_unit() {
        assert_eq!(fn1("abc", 5), "18f47a30");
    }

    #[test]
    fn test_fn1_input() {
        assert_eq!(fn1("cxdnnyjw", 5), "f77a0e6e");
    }

    #[test]
    fn test_fn2_unit() {
        assert_eq!(fn2("abc", 5), "05ace8e3");
    }

    #[test]
    fn test_fn2_input() {
        assert_eq!(fn2("cxdnnyjw", 5), "999828ec");
    }
}

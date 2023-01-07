extern crate core;

use std::collections::HashMap;

pub fn fn1(input: &str) -> i32 {
    input.lines().into_iter().filter(|s| is_nice1(s)).count() as i32
}

fn is_nice1(s: &str) -> bool {
    if s.contains("ab") || s.contains("cd") || s.contains("pq") || s.contains("xy") {
        return false;
    }

    let mut vowels = 0;
    let mut at_least_one_letter_appears_twice = false;
    let mut previous = ' ';
    for (i, c) in s.chars().into_iter().enumerate() {
        if is_vowel(&c) {
            vowels += 1;
        }

        if i != 0 {
            if c == previous {
                at_least_one_letter_appears_twice = true;
            }
        }
        previous = c;
    }

    vowels >= 3 && at_least_one_letter_appears_twice
}

fn is_vowel(c: &char) -> bool {
    match c {
        'a' | 'e' | 'i' | 'o' | 'u' => true,
        _ => false,
    }
}

pub fn fn2(input: &str) -> i32 {
    input.lines().into_iter().filter(|s| is_nice2(s)).count() as i32
}

fn is_nice2(s: &str) -> bool {
    contains_pair(s) && contains_one_repeat_letter(s)
}

fn contains_pair(s: &str) -> bool {
    let mut m: HashMap<String, i32> = HashMap::new();

    let mut i = 0;
    let mut to_check = true;
    while i < s.len() - 1 {
        if i > 0
            && to_check
            && s.chars().nth(i - 1) == s.chars().nth(i)
            && s.chars().nth(i) == s.chars().nth(i + 1)
        {
            i += 1;
            to_check = false;
            continue;
        }

        let str = format!(
            "{}{}",
            s.chars().nth(i).unwrap(),
            s.chars().nth(i + 1).unwrap()
        );
        let x = m.entry(str).or_insert(0);
        *x += 1;
        i += 1;
        to_check = true;
    }

    for (k, v) in &m {
        if v > &1 {
            return true;
        }
    }

    false
}

fn contains_one_repeat_letter(s: &str) -> bool {
    let mut n_1 = ' ';
    let mut n_2 = ' ';
    for (i, c) in s.chars().into_iter().enumerate() {
        if i > 1 {
            if c == n_2 {
                return true;
            }
        }
        n_2 = n_1;
        n_1 = c;
    }

    false
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn test_fn1_unit() {
        assert_eq!(fn1("ugknbfddgicrmopn"), 1);
        assert_eq!(fn1("aaa"), 1);
        assert_eq!(fn1("jchzalrnumimnmhp"), 0);
        assert_eq!(fn1("haegwjzuvuyypxyu"), 0);
        assert_eq!(fn1("dvszwmarrgswjxmb"), 0);
    }

    #[test]
    fn test_fn1_input() {
        let s = fs::read_to_string("input.txt").unwrap();
        assert_eq!(fn1(s.as_str()), 236);
    }

    #[test]
    fn test_fn2_unit() {
        assert_eq!(fn2("qjhvhtzxzqqjkmpb"), 1);
        assert_eq!(fn2("xxyxx"), 1);
        assert_eq!(fn2("uurcxstgmygtbstg"), 0);
        assert_eq!(fn2("ieodomkazucvgmuy"), 0);
        assert_eq!(contains_pair("aaa"), false);
        assert_eq!(contains_pair("aaaa"), true);
    }

    #[test]
    fn test_fn2_input() {
        let s = fs::read_to_string("input.txt").unwrap();
        assert_eq!(fn2(s.as_str()), 51);
    }
}

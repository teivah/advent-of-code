extern crate core;

use crate::Type::{
    Akitas, Cars, Cats, Children, Goldfish, Perfumes, Pomeranians, Samoyeds, Trees, Vizslas,
};
use std::collections::HashMap;

pub fn fn1(input: &str, m: HashMap<Type, i32>) -> i32 {
    let lines: Vec<_> = input.lines().collect();

    let mut sues = Vec::new();
    for s in lines.iter() {
        sues.push(Sue::new(s));
    }

    for sue in sues.iter() {
        let mut found = true;
        for (t, value) in m.iter() {
            if let Some(v) = sue.m.get(t) {
                if v != value {
                    found = false;
                    break;
                }
            }
        }
        if found {
            return sue.id;
        }
    }

    -1
}

#[derive(Debug)]
struct Sue {
    id: i32,
    m: HashMap<Type, i32>,
}

impl Sue {
    fn new(s: &str) -> Self {
        let idx = index_all(s, " ");
        let v = &s[idx[0] + 1..idx[1] - 1];
        let id = v.parse::<i32>().unwrap();

        let mut m = HashMap::new();

        if let Some(t) = find(s, "children: ") {
            m.insert(Children, t);
        }
        if let Some(t) = find(s, "cats: ") {
            m.insert(Cats, t);
        }
        if let Some(t) = find(s, "samoyeds: ") {
            m.insert(Samoyeds, t);
        }
        if let Some(t) = find(s, "pomeranians: ") {
            m.insert(Pomeranians, t);
        }
        if let Some(t) = find(s, "akitas: ") {
            m.insert(Akitas, t);
        }
        if let Some(t) = find(s, "vizslas: ") {
            m.insert(Vizslas, t);
        }
        if let Some(t) = find(s, "goldfish: ") {
            m.insert(Goldfish, t);
        }
        if let Some(t) = find(s, "trees: ") {
            m.insert(Trees, t);
        }
        if let Some(t) = find(s, "cars: ") {
            m.insert(Cars, t);
        }
        if let Some(t) = find(s, "perfumes: ") {
            m.insert(Perfumes, t);
        }

        Sue { id, m }
    }
}

fn find(s: &str, search: &str) -> Option<i32> {
    if let Some(t) = s.find(search) {
        if let Some(space) = s[t + search.len()..].find(" ") {
            let v = &s[t + search.len()..t + search.len() + space - 1];
            return Some(v.parse::<i32>().unwrap());
        } else {
            let v = &s[t + search.len()..];
            return Some(v.parse::<i32>().unwrap());
        }
    }

    None
}

#[derive(Eq, Hash, PartialEq, Debug)]
pub enum Type {
    Children,
    Cats,
    Samoyeds,
    Pomeranians,
    Akitas,
    Vizslas,
    Goldfish,
    Trees,
    Cars,
    Perfumes,
}

pub fn fn2(input: &str, m: HashMap<Type, i32>) -> i32 {
    let lines: Vec<_> = input.lines().collect();

    let mut sues = Vec::new();
    for s in lines.iter() {
        sues.push(Sue::new(s));
    }

    for sue in sues.iter() {
        let mut found = true;
        for (t, target) in m.iter() {
            if let Some(v) = sue.m.get(t) {
                match t {
                    Cats | Trees => {
                        if v <= target {
                            found = false;
                            break;
                        }
                    }
                    Pomeranians | Goldfish => {
                        if v >= target {
                            found = false;
                            break;
                        }
                    }
                    _ => {
                        if v != target {
                            found = false;
                            break;
                        }
                    }
                }
            }
        }
        if found {
            return sue.id;
        }
    }

    -1
}

fn index_all(s: &str, search: &str) -> Vec<usize> {
    let mut res = Vec::new();
    let mut i = 0;
    while i < s.len() {
        let idx = s[i..].find(search);
        match idx {
            None => return res,
            Some(t) => {
                res.push(t + i);
                i += t + search.len()
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
    fn test_index_all() {
        assert_eq!(index_all("abxxxabiiiabxx", "ab"), vec![0, 5, 10]);
    }

    #[test]
    fn test_fn1_input() {
        let mut m = HashMap::new();
        m.insert(Children, 3);
        m.insert(Cats, 7);
        m.insert(Samoyeds, 2);
        m.insert(Pomeranians, 3);
        m.insert(Akitas, 0);
        m.insert(Vizslas, 0);
        m.insert(Goldfish, 5);
        m.insert(Trees, 3);
        m.insert(Cars, 2);
        m.insert(Perfumes, 1);

        let s = fs::read_to_string("input.txt").unwrap();
        assert_eq!(fn1(s.as_str(), m), 373);
    }

    #[test]
    fn test_fn2_input() {
        let mut m = HashMap::new();
        m.insert(Children, 3);
        m.insert(Cats, 7);
        m.insert(Samoyeds, 2);
        m.insert(Pomeranians, 3);
        m.insert(Akitas, 0);
        m.insert(Vizslas, 0);
        m.insert(Goldfish, 5);
        m.insert(Trees, 3);
        m.insert(Cars, 2);
        m.insert(Perfumes, 1);

        let s = fs::read_to_string("input.txt").unwrap();
        assert_eq!(fn2(s.as_str(), m), 260);
    }
}

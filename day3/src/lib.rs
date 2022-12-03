use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::{BufRead, BufReader};

pub fn prioriy1(file: &str) -> Result<i32, Box<dyn std::error::Error>> {
    let reader = BufReader::new(File::open(file)?);

    let res = reader
        .lines()
        .map(|r| {
            let s = r.unwrap();
            let halves = s.split_at(s.len() / 2);
            let mut first: HashMap<char, i32> = HashMap::new();
            let mut second: HashMap<char, i32> = HashMap::new();
            halves.0.chars().for_each(|c| {
                *first.entry(c).and_modify(|v| *v += 1).or_insert(1);
            });
            halves.1.chars().for_each(|c| {
                *second.entry(c).and_modify(|v| *v += 1).or_insert(1);
            });

            first
                .iter()
                .map(|(key, _)| {
                    if second.contains_key(key) {
                        value(*key)
                    } else {
                        0
                    }
                })
                .fold(0, |sum, i| sum + i)
        })
        .fold(0, |sum, i| sum + i);

    Ok(res)
}

fn value(c: char) -> i32 {
    let v = if c >= 'A' && c <= 'Z' {
        c as u32 - 38
    } else {
        c as u32 - 96
    };
    v as i32
}

pub fn prioriy2(file: &str) -> Result<i32, Box<dyn std::error::Error>> {
    let reader = BufReader::new(File::open(file)?);
    let mut lines = vec![];
    reader.lines().for_each(|l| lines.push(l.unwrap()));

    let mut i = 0;
    let mut sum = 0;
    while i < lines.len() {
        let a = to_set(lines[i].as_str());
        let b = to_set(lines[i + 1].as_str());
        let c = to_set(lines[i + 2].as_str());

        sum += a
            .iter()
            .map(|key| {
                if b.contains(key) && c.contains(key) {
                    return value(*key);
                }
                0
            })
            .fold(0, |sum, i| sum + i);

        i += 3
    }

    Ok(sum)
}

fn to_set(line: &str) -> HashSet<char> {
    let mut set = HashSet::new();
    line.chars().for_each(|c| {
        set.insert(c);
    });
    set
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn priority1_unit() {
        let result = prioriy1("test.txt");
        assert_eq!(result.unwrap(), 157);
    }

    #[test]
    fn priority1_input() {
        let result = prioriy1("input.txt");
        assert_eq!(result.unwrap(), 8105);
    }

    #[test]
    fn priority2_unit() {
        let result = prioriy2("test.txt");
        assert_eq!(result.unwrap(), 70);
    }

    #[test]
    fn priority2_input() {
        let result = prioriy2("input.txt");
        assert_eq!(result.unwrap(), 2363);
    }
}

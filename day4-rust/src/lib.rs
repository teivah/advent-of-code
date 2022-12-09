use std::fs::{read, File};
use std::io::{BufRead, BufReader};
use std::ops::Index;

pub fn cleaning1(file: &str) -> Result<i32, Box<dyn std::error::Error>> {
    let reader = BufReader::new(File::open(file)?);

    Ok(reader
        .lines()
        .map(|s| {
            let line = s.unwrap();
            let idx = line.find(',').unwrap();
            let s = line.as_str();
            (get_range(&s[..idx]), get_range(&s[idx + 1..]))
        })
        .map(
            |(range1, range2)| {
                if is_contained(&range1, &range2) {
                    1
                } else {
                    0
                }
            },
        )
        .fold(0, |sum, i| sum + i))
}

fn is_contained(range1: &Range, range2: &Range) -> bool {
    (range1.start <= range2.start && range1.end >= range2.end)
        || (range2.start <= range1.start && range2.end >= range1.end)
}

fn get_range(s: &str) -> Range {
    let idx = s.find('-').unwrap();

    Range {
        start: s[..idx].parse().unwrap(),
        end: s[idx + 1..].parse().unwrap(),
    }
}

struct Range {
    start: i32,
    end: i32,
}

pub fn cleaning2(file: &str) -> Result<i32, Box<dyn std::error::Error>> {
    let reader = BufReader::new(File::open(file)?);

    Ok(reader
        .lines()
        .map(|s| {
            let line = s.unwrap();
            let idx = line.find(',').unwrap();
            let s = line.as_str();
            (get_range(&s[..idx]), get_range(&s[idx + 1..]))
        })
        .map(
            |(range1, range2)| {
                if is_overlap(&range1, &range2) {
                    1
                } else {
                    0
                }
            },
        )
        .fold(0, |sum, i| sum + i))
}

fn is_overlap(range1: &Range, range2: &Range) -> bool {
    (range1.start <= range2.start && range2.start <= range1.end)
        || (range2.start <= range1.start && range1.start <= range2.end)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cleaning1_unit() {
        let result = cleaning1("test.txt");
        assert_eq!(result.unwrap(), 2);
    }

    #[test]
    fn cleaning1_input() {
        let result = cleaning1("input.txt");
        assert_eq!(result.unwrap(), 526);
    }

    #[test]
    fn cleaning2_unit() {
        let result = cleaning2("test.txt");
        assert_eq!(result.unwrap(), 4);
    }

    #[test]
    fn cleaning2_input() {
        let result = cleaning2("input.txt");
        assert_eq!(result.unwrap(), 886);
    }
}

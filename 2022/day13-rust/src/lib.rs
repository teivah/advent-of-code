extern crate core;

use either::Either;
use std::cmp::Ordering;
use std::cmp::Ordering::{Greater, Less};

pub fn fn1(input: &str) -> i32 {
    let lines: Vec<_> = input.lines().collect();

    let mut iter = lines.iter();
    let mut res = 0;
    let mut pair = 1;

    while let Some(line1) = iter.next() {
        let line2 = iter.next().unwrap();

        let mut data1 = Vec::new();
        set(line1.as_bytes(), 1, &mut data1);
        let mut data2 = Vec::new();
        set(line2.as_bytes(), 1, &mut data2);

        let set1 = &Set { data: data1 };
        let set2 = &Set { data: data2 };

        if order(&set1.data, 0, &set2.data, 0) == 1 {
            res += pair;
        }

        // Empty line
        iter.next();
        pair += 1;
    }

    res
}

#[derive(Debug)]
struct Set {
    data: Vec<Either<Set, i32>>,
}

fn set(chars: &[u8], idx: usize, data: &mut Vec<Either<Set, i32>>) -> usize {
    let c = chars[idx];
    if c == '[' as u8 {
        let mut sub = Vec::new();
        let next = set(chars, idx + 1, &mut sub);
        data.push(Either::Left(Set { data: sub }));
        return set(chars, next, data);
    } else if c == ']' as u8 {
        return idx + 1;
    } else if c == ',' as u8 {
        return set(chars, idx + 1, data);
    } else {
        // If digit
        let mut i = idx + 1;
        loop {
            let c = chars[i];
            if !c.is_ascii_digit() {
                break;
            }
            i += 1;
        }
        let mut s = String::new();
        for v in idx..i {
            s.push(chars[v] as char)
        }
        let n = s.parse::<i32>().unwrap();
        data.push(Either::Right(n));
        return set(chars, idx + 1, data);
    }
}

fn order(
    left: &Vec<Either<Set, i32>>,
    ileft: usize,
    right: &Vec<Either<Set, i32>>,
    iright: usize,
) -> i32 {
    if left.len() == ileft && right.len() == iright {
        return 0;
    }

    if left.len() == ileft {
        return 1;
    }

    if right.len() == iright {
        return -1;
    }

    if left.get(ileft).unwrap().is_right() && right.get(iright).unwrap().is_right() {
        // Both integers
        let a = left.get(ileft).unwrap().as_ref().right().unwrap();
        let b = right.get(iright).unwrap().as_ref().right().unwrap();

        if *a < *b {
            return 1;
        } else if *b < *a {
            return -1;
        }
    } else if left.get(ileft).unwrap().is_left() && right.get(iright).unwrap().is_left() {
        // Both lists
        let a = left.get(ileft).unwrap().as_ref().left().unwrap();
        let b = right.get(iright).unwrap().as_ref().left().unwrap();
        let v = order(&a.data, 0, &b.data, 0);
        if v != 0 {
            return v;
        }
    } else if left.get(ileft).unwrap().is_right() {
        // Left only is integer
        let a = left.get(ileft).unwrap().as_ref().right().unwrap();
        let b = right.get(iright).unwrap().as_ref().left().unwrap();
        let v = order(&vec![Either::Right(*a)], 0, &b.data, 0);
        if v != 0 {
            return v;
        }
    } else {
        // Right only is integer
        let a = left.get(ileft).unwrap().as_ref().left().unwrap();
        let b = right.get(iright).unwrap().as_ref().right().unwrap();
        let v = order(&a.data, 0, &vec![Either::Right(*b)], 0);
        if v != 0 {
            return v;
        }
    }

    return order(left, ileft + 1, right, iright + 1);
}

pub fn fn2(input: &str) -> i32 {
    let lines: Vec<_> = input.lines().collect();

    let mut iter = lines.iter();
    let mut pairs = Vec::new();

    while let Some(line1) = iter.next() {
        let line2 = iter.next().unwrap();

        let mut data1 = Vec::new();
        set(line1.as_bytes(), 1, &mut data1);
        let mut data2 = Vec::new();
        set(line2.as_bytes(), 1, &mut data2);

        let set1 = Set { data: data1 };
        let set2 = Set { data: data2 };

        pairs.push(set1);
        pairs.push(set2);

        // Empty line
        iter.next();
    }

    pairs.push(Set {
        data: vec![Either::Left(Set {
            data: vec![Either::Right(2)],
        })],
    });
    pairs.push(Set {
        data: vec![Either::Left(Set {
            data: vec![Either::Right(6)],
        })],
    });

    pairs.sort();

    let divider_packet1 = Set {
        data: vec![Either::Left(Set {
            data: vec![Either::Right(2)],
        })],
    };
    let divider_packet2 = Set {
        data: vec![Either::Left(Set {
            data: vec![Either::Right(6)],
        })],
    };
    let mut idx1 = 0;
    let mut idx2 = 0;
    for i in 0..pairs.len() {
        let p = pairs.get(i).unwrap();
        if *p == divider_packet1 {
            idx1 = i + 1;
        } else if *p == divider_packet2 {
            idx2 = i + 1;
        }
    }

    idx1 as i32 * idx2 as i32
}

impl Eq for Set {}

impl PartialEq<Self> for Set {
    fn eq(&self, other: &Self) -> bool {
        order(&self.data, 0, &other.data, 0) == 0
    }
}

impl PartialOrd<Self> for Set {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Set {
    fn cmp(&self, other: &Self) -> Ordering {
        if order(&self.data, 0, &other.data, 0) == 1 {
            Less
        } else {
            Greater
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn test_fn1_unit() {
        let s = fs::read_to_string("test.txt").unwrap();
        assert_eq!(fn1(s.as_str()), 13);
    }

    #[test]
    fn test_fn1_input() {
        let s = fs::read_to_string("input.txt").unwrap();
        assert_eq!(fn1(s.as_str()), 4307);
    }

    #[test]
    fn test_fn2_unit() {
        let s = fs::read_to_string("test.txt").unwrap();
        assert_eq!(fn2(s.as_str()), 140);
    }

    #[test]
    fn test_fn2_input() {
        let s = fs::read_to_string("input.txt").unwrap();
        assert_eq!(fn2(s.as_str()), 21614);
    }
}

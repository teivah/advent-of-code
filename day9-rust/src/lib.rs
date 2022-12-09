extern crate core;

use std::collections::HashSet;

pub fn rope1(input: &str) -> i32 {
    let lines: Vec<_> = input.lines().collect();

    let mut visited = HashSet::new();

    let mut h = Position { x: 0, y: 0 };
    let mut t = Position { x: 0, y: 0 };
    visited.insert((0, 0));
    lines.iter().for_each(|line| {
        let m = get_move(line);

        for _ in 0..m.x.abs() {
            h.x += if m.x > 0 { 1 } else { -1 };
            t = tail_position(&h, &t);
            visited.insert((t.x, t.y));
        }

        for _ in 0..m.y.abs() {
            h.y += if m.y > 0 { 1 } else { -1 };
            t = tail_position(&h, &t);
            visited.insert((t.x, t.y));
        }
    });

    visited.len() as i32
}

#[derive(Eq, PartialEq)]
struct Position {
    x: i32,
    y: i32,
}

fn get_move(m: &str) -> Position {
    let s: Vec<&str> = m.split(' ').collect();
    let v: i32 = s[1].parse().unwrap();

    match s[0] {
        "R" => Position { x: v, y: 0 },
        "L" => Position { x: -v, y: 0 },
        "U" => Position { x: 0, y: v },
        "D" => Position { x: 0, y: -v },
        _ => Position { x: 0, y: 0 },
    }
}

fn tail_position(h: &Position, t: &Position) -> Position {
    // Same
    if h.x == t.x && h.y == t.y {
        return Position { x: h.x, y: h.y };
    }

    // Head is right
    if h.y == t.y && h.x > t.x {
        return Position { x: h.x - 1, y: t.y };
    }

    // Head is left
    if h.y == t.y && h.x < t.x {
        return Position { x: h.x + 1, y: t.y };
    }

    // Head is up
    if h.x == t.x && h.y > t.y {
        return Position { x: h.x, y: h.y - 1 };
    }

    // Head is down
    if h.x == t.x && h.y < t.y {
        return Position { x: h.x, y: h.y + 1 };
    }

    if is_touching(h, t) {
        return Position { x: t.x, y: t.y };
    }

    // Diagonally: up right
    if h.x > t.x && h.y > t.y {
        return Position {
            x: t.x + 1,
            y: t.y + 1,
        };
    }

    // Diagonally: up left
    if h.x < t.x && h.y > t.y {
        return Position {
            x: t.x - 1,
            y: t.y + 1,
        };
    }

    // Diagonally: bottom right
    if h.x > t.x && h.y < t.y {
        return Position {
            x: t.x + 1,
            y: t.y - 1,
        };
    }

    // Diagonally: bottom left
    if h.x < t.x && h.y < t.y {
        return Position {
            x: t.x - 1,
            y: t.y - 1,
        };
    }

    Position { x: 0, y: 0 }
}

fn is_touching(h: &Position, t: &Position) -> bool {
    (h.x - t.x).abs() == 1 && (h.y - t.y).abs() == 1
}

pub fn rope2(input: &str) -> i32 {
    let lines: Vec<_> = input.lines().collect();

    let mut visited = HashSet::new();

    let mut h = Position { x: 0, y: 0 };
    let mut knots = vec![];
    for _ in 0..9 {
        knots.push(Position { x: 0, y: 0 })
    }

    visited.insert((0, 0));
    lines.iter().for_each(|line| {
        let m = get_move(line);

        for _ in 0..m.x.abs() {
            h.x += if m.x > 0 { 1 } else { -1 };
            for i in 0..9 {
                let mut before = &h;
                if i > 0 {
                    before = &knots.get(i - 1).unwrap();
                }
                let res = tail_position(before, &knots.get(i).unwrap());
                let mut elem = &mut knots[i];
                elem.x = res.x;
                elem.y = res.y;
            }
            visited.insert((knots.get(8).unwrap().x, knots.get(8).unwrap().y));
        }

        for _ in 0..m.y.abs() {
            h.y += if m.y > 0 { 1 } else { -1 };
            for i in 0..9 {
                let mut before = &h;
                if i > 0 {
                    before = &knots.get(i - 1).unwrap();
                }
                let res = tail_position(before, &knots.get(i).unwrap());
                let mut elem = &mut knots[i];
                elem.x = res.x;
                elem.y = res.y;
            }
            visited.insert((knots.get(8).unwrap().x, knots.get(8).unwrap().y));
        }
    });

    visited.len() as i32
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn test_rope1_unit() {
        let s = fs::read_to_string("test.txt").unwrap();
        assert_eq!(rope1(s.as_str()), 13);
    }

    #[test]
    fn test_rope1_input() {
        let s = fs::read_to_string("input.txt").unwrap();
        assert_eq!(rope1(s.as_str()), 5245);
    }

    #[test]
    fn test_rope2_unit() {
        let s = fs::read_to_string("larger_test.txt").unwrap();
        assert_eq!(rope2(s.as_str()), 36);
    }

    #[test]
    fn test_rope2_input() {
        let s = fs::read_to_string("input.txt").unwrap();
        assert_eq!(rope2(s.as_str()), 2717);
    }
}

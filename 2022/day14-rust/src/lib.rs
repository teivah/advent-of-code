extern crate core;

use crate::Type::{AIR, ROCK, SAND};
use std::borrow::Borrow;
use std::io::Write;

pub fn fn1(input: &str) -> i32 {
    let lines: Vec<_> = input.lines().collect();

    let mut slices = Vec::new();
    let mut max = Pos {
        x: std::i32::MIN,
        y: std::i32::MIN,
    };
    let mut min = Pos {
        x: std::i32::MAX,
        y: std::i32::MAX,
    };
    lines.iter().for_each(|line| {
        let split = line.split(" -> ").collect::<Vec<_>>();
        let mut pos = Vec::new();
        split.iter().for_each(|coord| {
            let vec = coord
                .split(",")
                .map(|s| s.parse::<i32>().unwrap())
                .collect::<Vec<_>>();
            let x = vec[0];
            let y = vec[1];
            pos.push(Pos { x, y });
            if x < min.x {
                min.x = x;
            }
            if y < min.y {
                min.y = y;
            }
            if x > max.x {
                max.x = x;
            }
            if y > max.y {
                max.y = y;
            }
        });
        slices.push(Slice { pos });
    });

    // Sand pouring
    if 0 < min.y {
        min.y = 0;
    }

    let mut cave = Vec::new();
    for _ in min.y..=max.y {
        let mut row = Vec::new();
        for _ in min.x..=max.x {
            row.push(AIR);
        }
        cave.push(row);
    }

    slices.iter().for_each(|slice| {
        for i in 0..slice.pos.len() - 1 {
            let a = slice.pos[i].borrow();
            let b = slice.pos[i + 1].borrow();

            if b.x < a.x {
                // Left
                for x in b.x..=a.x {
                    let pos = get_position(x, b.y, &min);
                    cave[pos.y as usize][pos.x as usize] = ROCK;
                }
            } else if b.x > a.x {
                // Right
                for x in a.x..=b.x {
                    let pos = get_position(x, b.y, &min);
                    cave[pos.y as usize][pos.x as usize] = ROCK;
                }
            } else if b.y < a.y {
                // Down
                for y in b.y..=a.y {
                    let pos = get_position(b.x, y, &min);
                    cave[pos.y as usize][pos.x as usize] = ROCK;
                }
            } else {
                // Up
                for y in a.y..=b.y {
                    let pos = get_position(b.x, y, &min);
                    cave[pos.y as usize][pos.x as usize] = ROCK;
                }
            }
        }
    });

    let mut turn = 1;
    loop {
        let mut sand = Pos { x: 500, y: 1 };

        loop {
            let mut pos = get_position(sand.x, sand.y + 1, &min);
            if is_void(&pos, &min, &max) {
                return turn - 1;
            }
            if !is_blocked(&pos, &cave) {
                sand.y += 1;
                continue;
            }

            pos = get_position(sand.x - 1, sand.y + 1, &min);
            if is_void(&pos, &min, &max) {
                return turn - 1;
            }
            if !is_blocked(&pos, &cave) {
                sand.x -= 1;
                sand.y += 1;
                continue;
            }

            pos = get_position(sand.x + 1, sand.y + 1, &min);
            if is_void(&pos, &min, &max) {
                return turn - 1;
            }
            if !is_blocked(&pos, &cave) {
                sand.x += 1;
                sand.y += 1;
                continue;
            }

            break;
        }

        let pos = get_position(sand.x, sand.y, &min);
        cave[pos.y as usize][pos.x as usize] = SAND;

        // println!("turn: {}", turn);
        // display(&cave);
        turn += 1;
    }
}

fn is_void(pos: &Pos, min: &Pos, max: &Pos) -> bool {
    pos.x + min.x < min.x || pos.y + min.y < min.y || pos.x + min.x > max.x || pos.y + min.y > max.y
}

fn void_checker(pos: &mut Pos, cave: &mut Vec<Vec<Type>>, min: &mut Pos, max: &mut Pos) {
    if pos.x < 0 {
        for row in 0..cave.len() {
            if row == cave.len() - 1 {
                cave[row].insert(0, ROCK);
            } else {
                cave[row].insert(0, AIR);
            }
        }
        min.x -= 1;
        pos.x += 1
    } else if pos.x >= cave[0].len() as i32 {
        for row in 0..cave.len() {
            if row == cave.len() - 1 {
                cave[row].push(ROCK);
            } else {
                cave[row].push(AIR);
            }
        }
        max.x += 1;
    }
}

fn is_blocked(pos: &Pos, cave: &Vec<Vec<Type>>) -> bool {
    cave[pos.y as usize][pos.x as usize] != AIR
}

fn get_position(x: i32, y: i32, min: &Pos) -> Pos {
    Pos {
        x: x - min.x,
        y: y - min.y,
    }
}

fn display(cave: &Vec<Vec<Type>>) {
    for row in 0..cave.len() {
        for col in 0..cave[0].len() {
            match &cave[row][col] {
                AIR => print!("."),
                ROCK => print!("#"),
                SAND => print!("o"),
            }
        }
        println!()
    }
}

struct Slice {
    pos: Vec<Pos>,
}

struct Pos {
    x: i32,
    y: i32,
}

#[derive(PartialEq)]
enum Type {
    AIR,
    ROCK,
    SAND,
}

pub fn fn2(input: &str) -> i32 {
    let lines: Vec<_> = input.lines().collect();

    let mut slices = Vec::new();
    let mut max = Pos {
        x: std::i32::MIN,
        y: std::i32::MIN,
    };
    let mut min = Pos {
        x: std::i32::MAX,
        y: std::i32::MAX,
    };
    lines.iter().for_each(|line| {
        let split = line.split(" -> ").collect::<Vec<_>>();
        let mut pos = Vec::new();
        split.iter().for_each(|coord| {
            let vec = coord
                .split(",")
                .map(|s| s.parse::<i32>().unwrap())
                .collect::<Vec<_>>();
            let x = vec[0];
            let y = vec[1];
            pos.push(Pos { x, y });
            if x < min.x {
                min.x = x;
            }
            if y < min.y {
                min.y = y;
            }
            if x > max.x {
                max.x = x;
            }
            if y > max.y {
                max.y = y;
            }
        });
        slices.push(Slice { pos });
    });

    // Sand pouring
    if 0 < min.y {
        min.y = 0;
    }

    max.y += 2;

    let mut cave = Vec::new();
    for _ in min.y..=max.y {
        let mut row = Vec::new();
        for _ in min.x..=max.x {
            row.push(AIR);
        }
        cave.push(row);
    }

    slices.iter().for_each(|slice| {
        for i in 0..slice.pos.len() - 1 {
            let a = slice.pos[i].borrow();
            let b = slice.pos[i + 1].borrow();

            if b.x < a.x {
                // Left
                for x in b.x..=a.x {
                    let pos = get_position(x, b.y, &min);
                    cave[pos.y as usize][pos.x as usize] = ROCK;
                }
            } else if b.x > a.x {
                // Right
                for x in a.x..=b.x {
                    let pos = get_position(x, b.y, &min);
                    cave[pos.y as usize][pos.x as usize] = ROCK;
                }
            } else if b.y < a.y {
                // Down
                for y in b.y..=a.y {
                    let pos = get_position(b.x, y, &min);
                    cave[pos.y as usize][pos.x as usize] = ROCK;
                }
            } else {
                // Up
                for y in a.y..=b.y {
                    let pos = get_position(b.x, y, &min);
                    cave[pos.y as usize][pos.x as usize] = ROCK;
                }
            }
        }
    });

    let row = cave.len() - 1;
    for col in 0..cave[0].len() {
        cave[row][col] = ROCK;
    }

    let mut turn = 1;
    loop {
        let start_pos = get_position(500, 0, &min);
        if cave[start_pos.y as usize][start_pos.x as usize] == SAND {
            return turn - 1;
        }

        let mut sand = Pos { x: 500, y: 0 };

        loop {
            let mut pos = get_position(sand.x, sand.y + 1, &min);

            void_checker(&mut pos, &mut cave, &mut min, &mut max);
            if !is_blocked(&pos, &cave) {
                sand.y += 1;
                continue;
            }

            pos = get_position(sand.x - 1, sand.y + 1, &min);
            void_checker(&mut pos, &mut cave, &mut min, &mut max);
            if !is_blocked(&pos, &cave) {
                sand.x -= 1;
                sand.y += 1;
                continue;
            }

            pos = get_position(sand.x + 1, sand.y + 1, &min);
            void_checker(&mut pos, &mut cave, &mut min, &mut max);
            if !is_blocked(&pos, &cave) {
                sand.x += 1;
                sand.y += 1;
                continue;
            }

            break;
        }

        let pos = get_position(sand.x, sand.y, &min);
        cave[pos.y as usize][pos.x as usize] = SAND;

        // println!("turn: {}, {},{}", turn, pos.x, pos.y);
        // display(&cave);

        turn += 1;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn test_fn1_unit() {
        let s = fs::read_to_string("test.txt").unwrap();
        assert_eq!(fn1(s.as_str()), 24);
    }

    #[test]
    fn test_fn1_input() {
        let s = fs::read_to_string("input.txt").unwrap();
        assert_eq!(fn1(s.as_str()), 832);
    }

    #[test]
    fn test_fn2_unit() {
        let s = fs::read_to_string("test.txt").unwrap();
        assert_eq!(fn2(s.as_str()), 93);
    }

    #[test]
    fn test_fn2_input() {
        let s = fs::read_to_string("input.txt").unwrap();
        assert_eq!(fn2(s.as_str()), 27601);
    }
}

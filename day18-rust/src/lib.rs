extern crate core;

use std::collections::hash_map::Entry;
use std::collections::HashMap;

pub fn fn1(input: &str) -> i32 {
    let lines: Vec<_> = input.lines().collect();

    let mut grid = Grid::new();
    let mut blocks = Vec::new();
    for line in lines.iter() {
        let split = line.split(",").collect::<Vec<_>>();
        let x = split[0].parse::<i32>().unwrap();
        let y = split[1].parse::<i32>().unwrap();
        let z = split[2].parse::<i32>().unwrap();
        grid.add(x, y, z);
        blocks.push(Block { x, y, z })
    }

    let mut count = 0;
    for block in blocks.iter() {
        count += grid.is_empty(block.x - 1, block.y, block.z)
            + grid.is_empty(block.x + 1, block.y, block.z)
            + grid.is_empty(block.x, block.y - 1, block.z)
            + grid.is_empty(block.x, block.y + 1, block.z)
            + grid.is_empty(block.x, block.y, block.z - 1)
            + grid.is_empty(block.x, block.y, block.z + 1)
    }

    count
}

struct Block {
    x: i32,
    y: i32,
    z: i32,
}

struct Grid {
    grid: HashMap<i32, HashMap<i32, HashMap<i32, bool>>>,
}

impl Grid {
    fn new() -> Self {
        Grid {
            grid: HashMap::new(),
        }
    }

    fn add(&mut self, x: i32, y: i32, z: i32) {
        let m = self.grid.entry(x).or_insert(HashMap::new());
        let m = m.entry(y).or_insert(HashMap::new());
        m.insert(z, true);
    }

    fn is_empty(&mut self, x: i32, y: i32, z: i32) -> i32 {
        if let Entry::Occupied(v) = self.grid.entry(x) {
            let m = v.into_mut();
            if let Entry::Occupied(v) = m.entry(y) {
                let m = v.into_mut();
                if let Entry::Occupied(_) = m.entry(z) {
                    return 0;
                }
            }
        }

        1
    }
}

pub fn fn2(input: &str) -> i32 {
    let lines: Vec<_> = input.lines().collect();

    let mut grid = Grid::new();
    let mut blocks = Vec::new();
    for line in lines.iter() {
        let split = line.split(",").collect::<Vec<_>>();
        let x = split[0].parse::<i32>().unwrap();
        let y = split[1].parse::<i32>().unwrap();
        let z = split[2].parse::<i32>().unwrap();
        grid.add(x, y, z);
        blocks.push(Block { x, y, z })
    }

    1
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn test_fn1_unit() {
        let s = fs::read_to_string("test.txt").unwrap();
        assert_eq!(fn1(s.as_str()), 64);
    }

    #[test]
    fn test_fn1_input() {
        let s = fs::read_to_string("input.txt").unwrap();
        assert_eq!(fn1(s.as_str()), 3454);
    }

    #[test]
    fn test_fn2_unit() {
        let s = fs::read_to_string("test.txt").unwrap();
        assert_eq!(fn2(s.as_str()), 58);
    }

    #[test]
    fn test_fn2_input() {
        let s = fs::read_to_string("input.txt").unwrap();
        assert_eq!(fn2(s.as_str()), 1);
    }
}

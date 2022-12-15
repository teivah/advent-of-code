extern crate core;

use crate::Item::{BEACON, EMPTY, NO_BEACON, SENSOR};

pub fn fn1(input: &str, row: i32) -> i32 {
    let lines: Vec<_> = input.lines().collect();

    let mut sensors = Vec::new();
    let mut min_x = std::i32::MAX;
    let mut min_y = std::i32::MAX;
    let mut max_x = std::i32::MIN;
    let mut max_y = std::i32::MIN;
    for s in lines.iter() {
        // Sensor x
        let start = 12;
        let end = s.find(",").unwrap();
        let sensor_x = s[start..end].parse::<i32>().unwrap();

        // Sensor y
        let start = s[end..].find("=").unwrap() + 1 + end;
        let end = s[start..].find(":").unwrap() + start;
        let sensor_y = s[start..end].parse::<i32>().unwrap();

        // Beacon x
        let start = s[end..].find("=").unwrap() + 1 + end;
        let end = s[start..].find(",").unwrap() + start;
        let beacon_x = s[start..end].parse::<i32>().unwrap();

        // Beacon y
        let start = s[end..].find("=").unwrap() + 1 + end;
        let beacon_y = s[start..].parse::<i32>().unwrap();

        if sensor_x < min_x {
            min_x = sensor_x;
        }
        if sensor_y < min_y {
            min_y = sensor_y;
        }
        if beacon_x < min_x {
            min_x = beacon_x;
        }
        if beacon_y < min_y {
            min_y = beacon_y;
        }

        if sensor_x > max_x {
            max_x = sensor_x;
        }
        if sensor_y > max_y {
            max_y = sensor_y;
        }
        if beacon_x > max_x {
            max_x = beacon_x;
        }
        if beacon_y > max_y {
            max_y = beacon_y;
        }

        sensors.push(Sensor {
            position: Pos {
                x: sensor_x,
                y: sensor_y,
            },
            beacon: Pos {
                x: beacon_x,
                y: beacon_y,
            },
        });
    }

    let mut map = Map::new(min_x, min_y, max_x, max_y);

    for sensor in sensors.iter() {
        map.set(SENSOR, sensor.position.x, sensor.position.y);
        map.set(BEACON, sensor.beacon.x, sensor.beacon.y);
    }

    for sensor in sensors.iter() {
        map.expand(sensor.position.x, sensor.position.y);
        // map.print();
        // println!();
    }

    // map.print_row(row as usize);
    map.count_no_beacon(row)
}

struct Map {
    map: Vec<Vec<Item>>,
    min_x: i32,
    min_y: i32,
    max_x: i32,
    max_y: i32,
}

/*
..........
....#.....
...###....
..##S##...
...###....
....#.....
..........
 */

impl Map {
    fn new(min_x: i32, min_y: i32, max_x: i32, max_y: i32) -> Self {
        let mut map = Vec::new();

        for _ in min_y..=max_y {
            let mut line = Vec::new();
            for _ in min_x..=max_x {
                line.push(EMPTY);
            }
            map.push(line);
        }

        Map {
            map,
            min_x,
            min_y,
            max_x,
            max_y,
        }
    }

    fn expand(&mut self, x: i32, y: i32) {
        let idx_y = y - self.min_y;
        let idx_x = x - self.min_x;
        let mut count = 1;
        loop {
            let found = self.expand_count(idx_x, idx_y, count);
            if found {
                break;
            }
            count += 1;
        }
    }

    fn expand_count(&mut self, x: i32, y: i32, count: i32) -> bool {
        if count == 0 {
            return false;
        }

        let mut found = false;
        found = self.set_if_empty(x - 1, y) || found;
        found = self.set_if_empty(x + 1, y) || found;
        found = self.set_if_empty(x, y - 1) || found;
        found = self.set_if_empty(x, y + 1) || found;
        if found {
            return true;
        }

        let mut res = false;
        res = self.expand_count(x - 1, y, count - 1) || res;
        res = self.expand_count(x + 1, y, count - 1) || res;
        res = self.expand_count(x, y - 1, count - 1) || res;
        res = self.expand_count(x, y + 1, count - 1) || res;
        res
    }

    fn set_if_empty(&mut self, x: i32, y: i32) -> bool {
        if x < 0 || y < 0 || x as usize >= self.map[0].len() || y as usize >= self.map.len() {
            return false;
        }

        if self.map[y as usize][x as usize] == EMPTY {
            self.map[y as usize][x as usize] = NO_BEACON;
            return false;
        }

        if self.map[y as usize][x as usize] == BEACON {
            return true;
        }
        return false;
    }

    fn count_no_beacon(&self, y: i32) -> i32 {
        let mut count = 0;
        let idx_y = (y - self.min_y) as usize;
        for item in self.map[idx_y].iter() {
            if *item == NO_BEACON {
                count += 1;
            }
        }

        count
        // TODO improve using functional API
        // self.map[(y - self.min_y) as usize]
        //     .iter()
        //     .filter(|item| **item == NO_BEACON)
        //     .fold(0, |sum, _| sum + 1)
    }

    fn set(&mut self, item: Item, x: i32, y: i32) {
        self.map[(y - self.min_y) as usize][(x - self.min_x) as usize] = item;
    }

    fn get(&mut self, x: i32, y: i32) -> Item {
        self.map[(y - self.min_y) as usize][(x - self.min_x) as usize]
    }

    fn print(&self) {
        self.map.iter().for_each(|row| {
            row.iter().for_each(|item| match item {
                SENSOR => print!("S"),
                BEACON => print!("B"),
                EMPTY => print!("."),
                NO_BEACON => print!("#"),
            });
            println!()
        });
    }

    fn print_row(&self, y: usize) {
        self.map[y].iter().for_each(|item| match item {
            SENSOR => print!("S"),
            BEACON => print!("B"),
            EMPTY => print!("."),
            NO_BEACON => print!("#"),
        });
        println!()
    }
}

// ####B#####################..
// ####B######################..

#[derive(Copy, Clone, PartialEq, Debug)]
enum Item {
    SENSOR,
    BEACON,
    NO_BEACON,
    EMPTY,
}

struct Sensor {
    position: Pos,
    beacon: Pos,
}

struct Pos {
    x: i32,
    y: i32,
}

pub fn fn2(input: &str) -> i32 {
    let lines: Vec<_> = input.lines().collect();

    1
}

struct Row {
    data: Vec<Interval>,
}

impl Row {
    fn add(&mut self, x: i32, item: Item) {
        let mut res = Vec::new();
        let mut previous: Option<&Interval> = None;
        for i in 0..self.data.len() {
            let interval = &self.data[0];

            if let None = previous {
                previous = Some(interval);
            }
        }
        res.push(previous.unwrap());
        self.set(&res);
    }

    fn set(&mut self, data: &Vec<&Interval>) {
        self.data = Vec::new();
        for interval in data.iter() {
            self.data.push(**interval);
        }
    }

    fn print(&self) {
        for interval in self.data.iter() {
            println!("{:?}", interval);
        }
    }
}

#[derive(Copy, Clone, PartialEq, Debug)]
struct Interval {
    from: i32,
    to: i32,
    item: Item,
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn test_row() {
        let mut row = Row { data: vec![] };
        row.add(5, BEACON);
        row.add(10, BEACON);
        row.add(6, BEACON);
        row.add(8, BEACON);
        row.add(2, BEACON);
        row.add(12, BEACON);
        row.add(1, BEACON);
        row.add(13, BEACON);
        row.add(11, BEACON);
        row.print();
    }

    #[test]
    fn test_fn1_unit() {
        let s = fs::read_to_string("test.txt").unwrap();
        assert_eq!(fn1(s.as_str(), 10), 26);
    }

    #[test]
    fn test_fn1_input() {
        let s = fs::read_to_string("input.txt").unwrap();
        assert_eq!(fn1(s.as_str(), 2000000), 1);
    }

    #[test]
    fn test_fn2_unit() {
        let s = fs::read_to_string("test.txt").unwrap();
        assert_eq!(fn2(s.as_str()), 1);
    }

    #[test]
    fn test_fn2_input() {
        let s = fs::read_to_string("input.txt").unwrap();
        assert_eq!(fn2(s.as_str()), 1);
    }
}

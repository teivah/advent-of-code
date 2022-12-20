extern crate core;

pub fn fn1(input: &str, idx: Vec<usize>) -> i32 {
    let lines: Vec<_> = input.lines().map(|s| s.parse::<i32>().unwrap()).collect();
    let mut ring = Ring::new(lines);

    for i in 0..ring.len {
        ring.update(i);
        ring.print();
    }

    let mut sum = 0;
    for i in idx.iter() {
        sum += ring.get(*i);
    }

    1
}

pub fn fn2(input: &str) -> i32 {
    let lines: Vec<_> = input.lines().collect();

    1
}

struct Ring {
    data: Vec<i32>,
    pos: Vec<usize>,
    len: usize,
}

impl Ring {
    fn new(data: Vec<i32>) -> Self {
        let len = data.len();
        let mut pos = Vec::new();
        for i in 0..len {
            pos.push(i as usize);
        }
        Ring { data, pos, len }
    }

    fn get(&self, i: usize) -> i32 {
        let from = i.rem_euclid(self.len);
        self.data[from]
    }

    fn print(&self) {
        println!("{:?}", self.data);
    }

    fn update(&mut self, i: usize) {
        let from = self.pos[i];
        let value_from = self.data[from as usize];
        let to = (from as i32 + value_from).rem_euclid(self.len as i32) as usize;
        let value_to = self.data[to];

        self.data[from as usize] = value_to;
        self.data[to] = value_from;
        self.pos[i] = to;
        self.pos[to] = from;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn test_ring() {
        let mut r = Ring::new(vec![1, 2, -3, 3, -2, 0, 5]);

        r.update(2);
        assert_eq!(r.data[2], 5);
        assert_eq!(r.data[6], -3);
        assert_eq!(r.pos[2], 6);
        assert_eq!(r.pos[6], 2);

        // 1 2 5 3 -2 0 -3
        r.update(6);
        assert_eq!(r.data[2], 1);
        assert_eq!(r.data[0], 5);
        assert_eq!(r.pos[6], 0);
        assert_eq!(r.pos[0], 2);
    }

    #[test]
    fn test_fn1_unit() {
        let s = fs::read_to_string("test.txt").unwrap();
        assert_eq!(fn1(s.as_str(), vec![1000, 2000, 3000]), 3);
    }

    #[test]
    fn test_fn1_input() {
        let s = fs::read_to_string("input.txt").unwrap();
        assert_eq!(fn1(s.as_str(), vec![1000, 2000, 3000]), 1);
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

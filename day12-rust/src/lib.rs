extern crate core;

use priority_queue::PriorityQueue;
use std::borrow::Borrow;

pub fn fn1(input: &str) -> i32 {
    let lines: Vec<_> = input.lines().collect();

    let mut chars = Vec::new();
    for i in 0..lines.len() {
        let mut vec = Vec::new();
        let line = lines[i];
        line.chars().for_each(|c| vec.push(c));
        chars.push(vec);
    }

    let mut pq: PriorityQueue<_, _> = PriorityQueue::new();
    let mut nodes = Vec::new();
    let mut visited = Vec::new();
    for row in 0..chars.len() {
        let mut res = Vec::new();
        let mut visited_line = Vec::new();
        let lines = &chars[row];
        for col in 0..lines.len() {
            let mut left = None;
            let mut right = None;
            let mut top = None;
            let mut bottom = None;
            let elevation = to_elevation(lines[col]);
            let end = if lines[col] == 'E' { true } else { false };

            if row > 0 {
                if let Some(_) = chars.get(row - 1) {
                    top = Some(Pos {
                        row: row - 1,
                        col: col,
                    })
                }
            }

            if let Some(_) = chars.get(row + 1) {
                bottom = Some(Pos {
                    row: row + 1,
                    col: col,
                })
            }

            if col > 0 {
                if let Some(_) = lines.get(col - 1) {
                    left = Some(Pos {
                        row: row,
                        col: col - 1,
                    })
                }
            }

            if let Some(_) = lines.get(col + 1) {
                right = Some(Pos {
                    row: row,
                    col: col + 1,
                })
            }

            let node = Node {
                elevation,
                end,
                left,
                right,
                top,
                bottom,
                row,
                col,
            };
            res.push(node);

            if lines[col] == 'S' {
                visited_line.push(0);
                pq.push(node.clone(), 0);
            } else {
                visited_line.push(std::i32::MAX);
            }
        }
        nodes.push(res);
        visited.push(visited_line);
    }

    let mut min = std::i32::MAX;
    while !pq.is_empty() {
        let v = pq.pop().unwrap();
        let node = v.0;
        let distance = v.1;
        if node.end {
            if distance < min {
                min = distance;
            }
            continue;
        }

        // Left
        if node.col > 0 && visited[node.row][node.col - 1] > distance + 1 {
            if let Some(pos) = node.left {
                let n = &nodes[pos.row][pos.col];
                if n.elevation <= node.elevation + 1 {
                    pq.push(n.clone(), distance + 1);
                    visited[node.row][node.col - 1] = distance + 1;
                }
            }
        }

        // Right
        if node.col < visited[0].len() - 1 && visited[node.row][node.col + 1] > distance + 1 {
            if let Some(pos) = node.right {
                let n = &nodes[pos.row][pos.col];
                if n.elevation <= node.elevation + 1 {
                    pq.push(n.clone(), distance + 1);
                    visited[node.row][node.col + 1] = distance + 1;
                }
            }
        }

        // Top
        if node.row > 0 && visited[node.row - 1][node.col] > distance + 1 {
            if let Some(pos) = node.top {
                let n = &nodes[pos.row][pos.col];
                if n.elevation <= node.elevation + 1 {
                    pq.push(n.clone(), distance + 1);
                    visited[node.row - 1][node.col] = distance + 1;
                }
            }
        }

        // Bottom
        if node.row < visited.len() - 1 && visited[node.row + 1][node.col] > distance + 1 {
            if let Some(pos) = node.bottom {
                let n = &nodes[pos.row][pos.col];
                if n.elevation <= node.elevation + 1 {
                    pq.push(n.clone(), distance + 1);
                    visited[node.row + 1][node.col] = distance + 1;
                }
            }
        }
    }

    min
}

#[derive(PartialEq, Eq, Hash, Copy, Clone)]
struct Node {
    elevation: i32,
    end: bool,
    row: usize,
    col: usize,
    left: Option<Pos>,
    right: Option<Pos>,
    top: Option<Pos>,
    bottom: Option<Pos>,
}

#[derive(PartialEq, Eq, Hash, Copy, Clone)]
struct Pos {
    row: usize,
    col: usize,
}

fn to_elevation(c: char) -> i32 {
    if c == 'S' {
        1
    } else if c == 'E' {
        26
    } else {
        c.to_ascii_lowercase() as i32 - 'a' as i32 + 1
    }
}

pub fn fn2(input: &str) -> i32 {
    let lines: Vec<_> = input.lines().collect();

    let mut chars = Vec::new();
    for i in 0..lines.len() {
        let mut vec = Vec::new();
        let line = lines[i];
        line.chars().for_each(|c| vec.push(c));
        chars.push(vec);
    }

    let mut pq: PriorityQueue<_, _> = PriorityQueue::new();
    let mut nodes = Vec::new();
    let mut visited = Vec::new();
    let mut start = Vec::new();
    for row in 0..chars.len() {
        let mut res = Vec::new();
        let mut visited_line = Vec::new();
        let lines = &chars[row];
        for col in 0..lines.len() {
            let mut left = None;
            let mut right = None;
            let mut top = None;
            let mut bottom = None;
            let elevation = to_elevation(lines[col]);
            let end = if lines[col] == 'E' { true } else { false };

            if row > 0 {
                if let Some(_) = chars.get(row - 1) {
                    top = Some(Pos {
                        row: row - 1,
                        col: col,
                    })
                }
            }

            if let Some(_) = chars.get(row + 1) {
                bottom = Some(Pos {
                    row: row + 1,
                    col: col,
                })
            }

            if col > 0 {
                if let Some(_) = lines.get(col - 1) {
                    left = Some(Pos {
                        row: row,
                        col: col - 1,
                    })
                }
            }

            if let Some(_) = lines.get(col + 1) {
                right = Some(Pos {
                    row: row,
                    col: col + 1,
                })
            }

            let node = Node {
                elevation,
                end,
                left,
                right,
                top,
                bottom,
                row,
                col,
            };
            res.push(node);

            if lines[col] == 'S' || lines[col] == 'a' {
                visited_line.push(0);
                start.push(node.clone());
            } else {
                visited_line.push(std::i32::MAX);
            }
        }
        nodes.push(res);
        visited.push(visited_line);
    }

    let mut min = std::i32::MAX;
    for i in 0..start.len() {
        pq.push(start[i].clone(), 0);
        while !pq.is_empty() {
            let v = pq.pop().unwrap();
            let node = v.0;
            let distance = v.1;
            if node.end {
                if distance < min {
                    min = distance;
                }
                continue;
            }

            // Left
            if node.col > 0 && visited[node.row][node.col - 1] > distance + 1 {
                if let Some(pos) = node.left {
                    let n = &nodes[pos.row][pos.col];
                    if n.elevation <= node.elevation + 1 {
                        pq.push(n.clone(), distance + 1);
                        visited[node.row][node.col - 1] = distance + 1;
                    }
                }
            }

            // Right
            if node.col < visited[0].len() - 1 && visited[node.row][node.col + 1] > distance + 1 {
                if let Some(pos) = node.right {
                    let n = &nodes[pos.row][pos.col];
                    if n.elevation <= node.elevation + 1 {
                        pq.push(n.clone(), distance + 1);
                        visited[node.row][node.col + 1] = distance + 1;
                    }
                }
            }

            // Top
            if node.row > 0 && visited[node.row - 1][node.col] > distance + 1 {
                if let Some(pos) = node.top {
                    let n = &nodes[pos.row][pos.col];
                    if n.elevation <= node.elevation + 1 {
                        pq.push(n.clone(), distance + 1);
                        visited[node.row - 1][node.col] = distance + 1;
                    }
                }
            }

            // Bottom
            if node.row < visited.len() - 1 && visited[node.row + 1][node.col] > distance + 1 {
                if let Some(pos) = node.bottom {
                    let n = &nodes[pos.row][pos.col];
                    if n.elevation <= node.elevation + 1 {
                        pq.push(n.clone(), distance + 1);
                        visited[node.row + 1][node.col] = distance + 1;
                    }
                }
            }
        }
    }

    min
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn test_fn1_unit() {
        let s = fs::read_to_string("test.txt").unwrap();
        assert_eq!(fn1(s.as_str()), 31);
    }

    #[test]
    fn test_fn1_input() {
        let s = fs::read_to_string("input.txt").unwrap();
        assert_eq!(fn1(s.as_str()), 698);
    }

    #[test]
    fn test_fn2_unit() {
        let s = fs::read_to_string("test.txt").unwrap();
        assert_eq!(fn2(s.as_str()), 29);
    }

    #[test]
    fn test_fn2_input() {
        let s = fs::read_to_string("input.txt").unwrap();
        assert_eq!(fn2(s.as_str()), 500);
    }
}

use queues::*;

use std::collections::HashSet;

pub fn fn1(number: i64, destination: (i64, i64)) -> i64 {
    let mut visited = HashSet::new();
    let mut q: Queue<_> = queue![];
    let _ = q.add(State {
        pos: (1, 1),
        moves: 0,
    });

    for row in 0..6 {
        for col in 0..10 {
            if is_wall(number, (col, row)) {
                print!("#");
            } else {
                print!(".");
            }
        }
        println!();
    }

    loop {
        let state = q.remove().unwrap();
        let pos = state.pos;
        if pos == destination {
            return state.moves;
        }

        if visited.contains(&pos) {
            continue;
        }

        visited.insert(pos);

        if let Some(p) = is_valid_position(&pos, (0, -1), &visited, number) {
            let _ = q.add(State {
                pos: p,
                moves: state.moves + 1,
            });
        }

        if let Some(p) = is_valid_position(&pos, (-1, 0), &visited, number) {
            let _ = q.add(State {
                pos: p,
                moves: state.moves + 1,
            });
        }

        if let Some(p) = is_valid_position(&pos, (1, 0), &visited, number) {
            let _ = q.add(State {
                pos: p,
                moves: state.moves + 1,
            });
        }

        if let Some(p) = is_valid_position(&pos, (0, 1), &visited, number) {
            let _ = q.add(State {
                pos: p,
                moves: state.moves + 1,
            });
        }
    }
}

#[derive(Clone)]
struct State {
    pos: (i64, i64),
    moves: i64,
}

fn is_valid_position(
    pos: &(i64, i64),
    delta: (i64, i64),
    visited: &HashSet<(i64, i64)>,
    number: i64,
) -> Option<(i64, i64)> {
    let x = pos.0 + delta.0;
    let y = pos.1 + delta.1;
    if x < 0 || y < 0 {
        return None;
    }

    if is_wall(number, (x, y)) {
        return None;
    }

    if visited.contains(&(x, y)) {
        return None;
    }

    Some((x, y))
}

fn is_wall(number: i64, pos: (i64, i64)) -> bool {
    let x = pos.0;
    let y = pos.1;

    let mut v = x * x + 3 * x + 2 * x * y + y + y * y + number;
    let mut ones = 0;
    while v != 0 {
        if v % 2 == 1 {
            ones += 1;
        }
        v >>= 1
    }
    ones % 2 == 1
}

pub fn fn2(number: i64, steps: i64) -> i64 {
    let mut visited = HashSet::new();
    let mut q: Queue<_> = queue![];
    let _ = q.add(State {
        pos: (1, 1),
        moves: 0,
    });

    loop {
        match q.remove() {
            Err(_) => break,
            Ok(state) => {
                if state.moves > steps {
                    continue;
                }
                let pos = state.pos;

                if visited.contains(&pos) {
                    continue;
                }
                visited.insert(pos);

                if let Some(p) = is_valid_position(&pos, (0, -1), &visited, number) {
                    let _ = q.add(State {
                        pos: p,
                        moves: state.moves + 1,
                    });
                }

                if let Some(p) = is_valid_position(&pos, (-1, 0), &visited, number) {
                    let _ = q.add(State {
                        pos: p,
                        moves: state.moves + 1,
                    });
                }

                if let Some(p) = is_valid_position(&pos, (1, 0), &visited, number) {
                    let _ = q.add(State {
                        pos: p,
                        moves: state.moves + 1,
                    });
                }

                if let Some(p) = is_valid_position(&pos, (0, 1), &visited, number) {
                    let _ = q.add(State {
                        pos: p,
                        moves: state.moves + 1,
                    });
                }
            }
        }
    }

    visited.len() as i64
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn test_fn1_unit() {
        assert_eq!(fn1(10, (7, 4)), 11);
    }

    #[test]
    fn test_fn1_input() {
        assert_eq!(fn1(1358, (31, 39)), 96);
    }

    #[test]
    fn test_fn2_input() {
        assert_eq!(fn2(1358, 50), 1);
    }
}

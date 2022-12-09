use std::collections::HashSet;

pub fn visible1(input: &str) -> i64 {
    let lines: Vec<_> = input.lines().collect();
    let trees: Vec<Vec<u32>> = lines
        .iter()
        .map(|line| line.chars().map(|c| c.to_digit(10).unwrap()).collect())
        .collect();

    let nb_row = trees.len();
    let nb_col = trees.get(0).unwrap().len();
    let mut set = HashSet::new();

    for col in 0..nb_col {
        let mut max = 0;
        for row in 0..nb_row {
            let v = *trees.get(row).unwrap().get(col).unwrap();
            if v > max || row == 0 {
                set.insert((row, col));
                max = v;
            }
        }
    }

    for row in 0..nb_row {
        let mut max = 0;
        for col in 0..nb_col {
            let v = *trees.get(row).unwrap().get(col).unwrap();
            if v > max || col == 0 {
                set.insert((row, col));
                max = v;
            }
        }
    }

    for col in 0..nb_col {
        let mut max = 0;
        for row in (0..nb_row).rev() {
            let v = *trees.get(row).unwrap().get(col).unwrap();
            if v > max || row == nb_row - 1 {
                set.insert((row, col));
                max = v;
            }
        }
    }

    for row in 0..nb_row {
        let mut max = 0;
        for col in (0..nb_col).rev() {
            let v = *trees.get(row).unwrap().get(col).unwrap();
            if v > max || col == nb_col - 1 {
                set.insert((row, col));
                max = v;
            }
        }
    }

    set.len() as i64
}

pub fn visible2(input: &str) -> i64 {
    let lines: Vec<_> = input.lines().collect();
    let trees: Vec<Vec<u32>> = lines
        .iter()
        .map(|line| line.chars().map(|c| c.to_digit(10).unwrap()).collect())
        .collect();

    let nb_row = trees.len();
    let nb_col = trees.get(0).unwrap().len();

    let mut max_score = 0;
    for r in 0..nb_row {
        for c in 0..nb_col {
            let current_tree = *trees.get(r).unwrap().get(c).unwrap();
            let mut score = 1;

            // Left
            let mut count = 0;
            for col in (0..c).rev() {
                let n = *trees.get(r).unwrap().get(col).unwrap();
                count += 1;
                if n >= current_tree {
                    break;
                }
            }
            score *= count;

            // Right
            let mut count = 0;
            for col in c + 1..nb_col {
                let n = *trees.get(r).unwrap().get(col).unwrap();
                count += 1;
                if n >= current_tree {
                    break;
                }
            }
            score *= count;

            // Top
            let mut count = 0;
            for row in (0..r).rev() {
                let n = *trees.get(row).unwrap().get(c).unwrap();
                count += 1;
                if n >= current_tree {
                    break;
                }
            }
            score *= count;

            // Bottom
            let mut count = 0;
            for row in r + 1..nb_row {
                let n = *trees.get(row).unwrap().get(c).unwrap();
                count += 1;
                if n >= current_tree {
                    break;
                }
            }
            score *= count;

            if score > max_score {
                max_score = score;
            }
        }
    }

    max_score
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn test_visible1_unit() {
        let s = fs::read_to_string("test.txt").unwrap();
        assert_eq!(visible1(s.as_str()), 21);
    }

    #[test]
    fn test_visible1_input() {
        let s = fs::read_to_string("input.txt").unwrap();
        assert_eq!(visible1(s.as_str()), 1679);
    }

    #[test]
    fn test_visible2_unit() {
        let s = fs::read_to_string("test.txt").unwrap();
        assert_eq!(visible2(s.as_str()), 8);
    }

    #[test]
    fn test_visible2_input() {
        let s = fs::read_to_string("input.txt").unwrap();
        assert_eq!(visible2(s.as_str()), 536625);
    }
}

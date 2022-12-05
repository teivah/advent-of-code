use regex::Regex;
use std::borrow::BorrowMut;
use std::error::Error;
use std::fs;

pub fn supply1(input: &str) -> Result<String, Box<&'static str>> {
    let mut lines = vec![];
    input.lines().for_each(|line| lines.push(line));

    let mut line_idx = 0;
    let mut max = 0;
    loop {
        let len = lines[line_idx].len();
        if len > max {
            max = len;
        }
        if lines[line_idx].chars().nth(1).unwrap() == '1' {
            break;
        }
        line_idx += 1;
    }
    let move_idx = line_idx + 2;
    line_idx -= 1;

    let nb_stacks = get_number_stacks(max);

    let mut stacks = Box::new(Vec::new());
    for i in 0..nb_stacks {
        stacks.push(Vec::new());
    }

    loop {
        let mut char_index = 1;
        for stack_idx in 0..nb_stacks {
            if let Some(c) = lines[line_idx].chars().nth(char_index) {
                if c != ' ' {
                    (*stacks).get_mut(stack_idx).unwrap().push(c);
                }
            } else {
                break;
            }
            char_index += 4;
        }

        if line_idx == 0 {
            break;
        }
        line_idx -= 1;
    }

    let rx = Regex::new(r"move (\d+) from (\d+) to (\d+)").unwrap();
    let mut moves = vec![];
    for i in move_idx..lines.len() {
        let caps = rx.captures(lines[i]).unwrap();
        let count = caps
            .get(1)
            .map_or("", |m| m.as_str())
            .parse::<i32>()
            .unwrap();
        let from = caps
            .get(2)
            .map_or("", |m| m.as_str())
            .parse::<i32>()
            .unwrap();
        let to = caps
            .get(3)
            .map_or("", |m| m.as_str())
            .parse::<i32>()
            .unwrap();
        moves.push(Move {
            count,
            from: from as usize,
            to: to as usize,
        })
    }

    for i in 0..moves.len() {
        let m = moves.get(i).unwrap();
        for count in 0..m.count {
            let c: char;
            {
                let stack = (*stacks).get_mut(m.from - 1).unwrap();
                c = *stack.last().unwrap();
                stack.remove(stack.len() - 1);
            }
            (*stacks).get_mut(m.to - 1).unwrap().push(c);
        }
    }

    let mut res: String = String::from("");
    for i in 0..stacks.len() {
        let stack = (*stacks).get_mut(i).unwrap();
        res.push(*stack.last().unwrap());
    }

    Ok(res)
}

pub fn supply2(input: &str) -> Result<String, Box<&'static str>> {
    let mut lines = vec![];
    input.lines().for_each(|line| lines.push(line));

    let mut line_idx = 0;
    let mut max = 0;
    loop {
        let len = lines[line_idx].len();
        if len > max {
            max = len;
        }
        if lines[line_idx].chars().nth(1).unwrap() == '1' {
            break;
        }
        line_idx += 1;
    }
    let move_idx = line_idx + 2;
    line_idx -= 1;

    let nb_stacks = get_number_stacks(max);

    let mut stacks = Box::new(Vec::new());
    for i in 0..nb_stacks {
        stacks.push(Vec::new());
    }

    loop {
        let mut char_index = 1;
        for stack_idx in 0..nb_stacks {
            if let Some(c) = lines[line_idx].chars().nth(char_index) {
                if c != ' ' {
                    (*stacks).get_mut(stack_idx).unwrap().push(c);
                }
            } else {
                break;
            }
            char_index += 4;
        }

        if line_idx == 0 {
            break;
        }
        line_idx -= 1;
    }

    let rx = Regex::new(r"move (\d+) from (\d+) to (\d+)").unwrap();
    let mut moves = vec![];
    for i in move_idx..lines.len() {
        let caps = rx.captures(lines[i]).unwrap();
        let count = caps
            .get(1)
            .map_or("", |m| m.as_str())
            .parse::<i32>()
            .unwrap();
        let from = caps
            .get(2)
            .map_or("", |m| m.as_str())
            .parse::<i32>()
            .unwrap();
        let to = caps
            .get(3)
            .map_or("", |m| m.as_str())
            .parse::<i32>()
            .unwrap();
        moves.push(Move {
            count,
            from: from as usize,
            to: to as usize,
        })
    }

    for i in 0..moves.len() {
        let m = moves.get(i).unwrap();
        let stack = (*stacks).get_mut(m.from - 1).unwrap();
        let idx = stack.len() - m.count as usize;
        for count in 0..m.count {
            let c: char;
            {
                let stack = (*stacks).get_mut(m.from - 1).unwrap();
                c = stack.remove(idx);
            }
            (*stacks).get_mut(m.to - 1).unwrap().push(c);
        }
    }

    let mut res: String = String::from("");
    for i in 0..stacks.len() {
        let stack = (*stacks).get_mut(i).unwrap();
        res.push(*stack.last().unwrap());
    }

    Ok(res)
}

struct Move {
    count: i32,
    from: usize,
    to: usize,
}

fn get_number_stacks(max: usize) -> usize {
    (max + 1) / 4
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn supply1_unit() -> Result<(), Box<dyn Error>> {
        let s = fs::read_to_string("test.txt")?;
        let result = supply1(s.as_str());
        assert_eq!(result.unwrap(), String::from("CMZ"));
        Ok(())
    }

    #[test]
    fn supply1_input() -> Result<(), Box<dyn Error>> {
        let s = fs::read_to_string("input.txt")?;
        let result = supply1(s.as_str());
        assert_eq!(result.unwrap(), String::from("VPCDMSLWJ"));
        Ok(())
    }

    #[test]
    fn supply2_unit() -> Result<(), Box<dyn Error>> {
        let s = fs::read_to_string("test.txt")?;
        let result = supply2(s.as_str());
        assert_eq!(result.unwrap(), String::from("MCD"));
        Ok(())
    }

    #[test]
    fn supply2_input() -> Result<(), Box<dyn Error>> {
        let s = fs::read_to_string("input.txt")?;
        let result = supply2(s.as_str());
        assert_eq!(result.unwrap(), String::from("TPWCGNCCG"));
        Ok(())
    }
}

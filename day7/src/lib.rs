use std::borrow::{Borrow, BorrowMut};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub fn disk1(input: &str) -> i64 {
    let commands: Vec<_> = input.lines().collect();

    let mut root = Node {
        size: 0,
        total_size: 0,
        children: HashMap::new(),
        parent: None,
    };

    run_through(commands, 0, &mut root);

    1
}

fn run_through(commands: Vec<&str>, idx: usize, current: &mut Node) {
    if idx == commands.len() {
        return;
    }

    let command = *commands.get(idx).unwrap();

    if command == "$ cd .." {
        run_through(commands, idx + 1, current.parent.unwrap())
    } else if command.starts_with("$ cd") {
        let children = current.children.get(&command[5..]).unwrap();
        run_through(commands, idx + 1, &mut children);
    }
}

struct Node {
    size: i64,
    total_size: i64, // size + all the children
    children: HashMap<String, Rc<RefCell<Node>>>,
    parent: Option<Rc<RefCell<Node>>>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn test_disk1_unit() {
        let s = fs::read_to_string("test.txt").unwrap();
        assert_eq!(disk1(s.as_str()), 95437);
    }

    #[test]
    fn test_disk1_input() {
        let s = fs::read_to_string("input.txt").unwrap();
        assert_eq!(disk1(s.as_str()), 95437);
    }
}

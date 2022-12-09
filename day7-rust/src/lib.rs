use std::borrow::{Borrow, BorrowMut};
use std::cell::{Ref, RefCell};
use std::collections::HashMap;
use std::rc::{Rc, Weak};

pub fn disk1(input: &str) -> i64 {
    let commands: Vec<_> = input.lines().collect();

    let mut root = Node {
        size: 0,
        total_size: 0,
        children: RefCell::new(HashMap::new()),
        parent: RefCell::new(Weak::new()),
    };

    // run_through(commands, 0, Rc::new(RefCell::new(Node)));
    run_through(commands, 0, &root);

    1
}

fn run_through(commands: Vec<&str>, idx: usize, current: &Node) {
    if idx == commands.len() {
        return;
    }

    let command = *commands.get(idx).unwrap();

    if command == "$ cd .." {
        let current1 = current.parent.borrow().upgrade().unwrap().borrow();
        run_through(commands, idx + 1, current1)
    }
}

struct Node {
    size: i64,
    total_size: i64, // size + all the children
    children: RefCell<HashMap<String, Rc<Node>>>,
    parent: RefCell<Weak<Node>>,
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

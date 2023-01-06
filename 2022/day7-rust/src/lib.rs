use std::cell::{Ref, RefCell, RefMut};
use std::collections::HashMap;
use std::rc::{Rc, Weak};

pub fn disk1(input: &str) -> i64 {
    let commands: Vec<_> = input.lines().collect();

    let mut root = Node {
        size: 0,
        total_size: 0,
        children: HashMap::new(),
        parent: None,
    };
    let mut current: &mut Node = &mut node;

    commands.iter().for_each(|line| {
        if line == "$ cd /" {
            current = &mut root;
        } else if line == "$ cd .." {
            // current = &mut current.parent.unwrap().borrow_mut();
        }
    });

    run_through(commands, 0, &mut root);

    1
}

fn run_through(commands: Vec<&str>, idx: usize, current: &mut Node) {
    let mut x = current.children.get("a").unwrap().borrow_mut();
    // x.size = 1;
    run_through(commands, idx, &mut x);
}

struct Node {
    size: i64,
    total_size: i64, // size + all the children
    children: HashMap<String, Rc<RefCell<Node>>>,
    parent: Option<Rc<RefCell<Node>>>,
}

impl Node {
    fn add_child(&mut self, name:String, node: Node) {
        self.children.insert(name, Rc::new(RefCell::new(node)));
    }
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

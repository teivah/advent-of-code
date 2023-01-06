extern crate core;

pub fn monkey1(input: &str) -> i32 {
    let lines: Vec<_> = input.lines().collect();
    let mut iter = lines.iter();

    let mut monkeys = Vec::new();
    loop {
        // Monkey x
        let first = iter.next();
        match first {
            None => break,
            _ => (),
        }

        // Starting items
        let s = &iter.next().unwrap()[18..];
        let items = s
            .split(", ")
            .into_iter()
            .map(|s| s.parse::<i128>().unwrap())
            .collect::<Vec<_>>();

        // Operation
        let s = iter.next().unwrap();
        let operator = &s[23..24];
        let operand = &s[25..];

        // Divisible
        let s = iter.next().unwrap();
        let divisible = s[21..].parse::<i32>().unwrap();

        // If true
        let s = iter.next().unwrap();
        let if_true = s[29..].parse::<usize>().unwrap();

        // If false
        let s = iter.next().unwrap();
        let if_false = s[30..].parse::<usize>().unwrap();

        monkeys.push(Monkey {
            divisible,
            if_true,
            if_false,
            operator,
            operand,
            items,
        });

        iter.next();
    }

    let mut inspected = Vec::new();
    monkeys.iter().for_each(|_| inspected.push(0));

    for _ in 0..20 {
        for i in 0..monkeys.len() {
            let monkey = &mut monkeys[i];
            let mut moves: Vec<_> = Vec::new();
            while !monkey.items.is_empty() {
                inspected[i] += 1;
                let worry_level = monkey.items.remove(0);
                let new_worry_level = monkey.worry_level(worry_level);
                let idx = monkey.next_monkey(new_worry_level) as usize;
                moves.push((idx, new_worry_level));
            }

            moves.iter().for_each(|m| {
                monkeys[m.0].items.push(m.1);
            })
        }
    }

    inspected.sort_by(|a, b| b.cmp(a));
    inspected[0] * inspected[1]
}

pub fn monkey2(input: &str) -> i128 {
    let lines: Vec<_> = input.lines().collect();
    let mut iter = lines.iter();

    let mut monkeys = Vec::new();
    loop {
        // Monkey x
        let first = iter.next();
        match first {
            None => break,
            _ => (),
        }

        // Starting items
        let s = &iter.next().unwrap()[18..];
        let items = s
            .split(", ")
            .into_iter()
            .map(|s| s.parse::<i128>().unwrap())
            .collect::<Vec<_>>();

        // Operation
        let s = iter.next().unwrap();
        let operator = &s[23..24];
        let operand = &s[25..];

        // Divisible
        let s = iter.next().unwrap();
        let divisible = s[21..].parse::<i32>().unwrap();

        // If true
        let s = iter.next().unwrap();
        let if_true = s[29..].parse::<usize>().unwrap();

        // If false
        let s = iter.next().unwrap();
        let if_false = s[30..].parse::<usize>().unwrap();

        monkeys.push(Monkey {
            divisible,
            if_true,
            if_false,
            operator,
            operand,
            items,
        });

        iter.next();
    }

    let max: i128 = monkeys.iter().fold(1, |max, i| max * i.divisible as i128);

    let mut inspected = Vec::new();
    monkeys.iter().for_each(|_| inspected.push(0));

    for _ in 0..10000 {
        for i in 0..monkeys.len() {
            let monkey = &mut monkeys[i];
            let mut moves: Vec<_> = Vec::new();
            while !monkey.items.is_empty() {
                inspected[i] += 1;
                let worry_level = monkey.items.remove(0);
                let new_worry_level = monkey.worry_level_max(worry_level, max);
                let idx = monkey.next_monkey(new_worry_level) as usize;
                moves.push((idx, new_worry_level));
            }

            moves.iter().for_each(|m| {
                monkeys[m.0].items.push(m.1);
            })
        }
    }

    inspected.sort_by(|a, b| b.cmp(a));
    inspected[0] as i128 * inspected[1] as i128
}

struct Monkey<'a> {
    divisible: i32,
    if_true: usize,
    if_false: usize,
    operator: &'a str,
    operand: &'a str,
    items: Vec<i128>,
}

impl<'a> Monkey<'a> {
    fn worry_level(&self, old: i128) -> i128 {
        let after = if self.operand == "old" {
            match self.operator {
                "*" => old * old,
                "+" => old + old,
                _ => panic!("unknown operator"),
            }
        } else {
            let v = self.operand.parse::<i128>().unwrap();
            match self.operator {
                "*" => old * v,
                "+" => old + v,
                _ => panic!("unknown operator"),
            }
        };
        after / 3
    }

    fn worry_level_max(&self, old: i128, max: i128) -> i128 {
        let after = if self.operand == "old" {
            match self.operator {
                "*" => (old * old) % max,
                "+" => (old + old) % max,
                _ => panic!("unknown operator"),
            }
        } else {
            let v = self.operand.parse::<i128>().unwrap();
            match self.operator {
                "*" => (old * v) % max,
                "+" => (old + v) % max,
                _ => panic!("unknown operator"),
            }
        };
        after
    }

    fn next_monkey(&self, worry_level: i128) -> usize {
        if worry_level % self.divisible as i128 == 0 {
            self.if_true
        } else {
            self.if_false
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn test_monkey1_unit() {
        let s = fs::read_to_string("test.txt").unwrap();
        assert_eq!(monkey1(s.as_str()), 10605);
    }

    #[test]
    fn test_monkey1_input() {
        let s = fs::read_to_string("input.txt").unwrap();
        assert_eq!(monkey1(s.as_str()), 57348);
    }

    #[test]
    fn test_monkey2_unit() {
        let s = fs::read_to_string("test.txt").unwrap();
        assert_eq!(monkey2(s.as_str()), 2713310158);
    }

    #[test]
    fn test_monkey2_input() {
        let s = fs::read_to_string("input.txt").unwrap();
        assert_eq!(monkey2(s.as_str()), 14106266886);
    }
}

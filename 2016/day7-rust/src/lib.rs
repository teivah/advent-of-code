extern crate core;

pub fn fn1(input: &str) -> i32 {
    let lines: Vec<_> = input.lines().collect();

    lines
        .iter()
        .filter(|s| {
            let vstart = index_all(s, "[");
            let vend = index_all(s, "]");

            let mut one_abba = false;
            for i in 0..vstart.len() {
                let start = vstart[i];
                let end = vend[i];

                if abba(&s[start + 1..end]) {
                    return false;
                }

                if start != 0 {
                    if i == 0 {
                        if abba(&s[..start]) {
                            one_abba = true;
                        }
                    } else {
                        if abba(&s[vend[i - 1] + 1..start]) {
                            one_abba = true;
                        }
                    }
                }
            }

            if vend[vend.len() - 1] != s.len() - 1 {
                if abba(&s[vend[vend.len() - 1] + 1..]) {
                    one_abba = true;
                }
            }

            one_abba
        })
        .count() as i32
}

fn abba(s: &str) -> bool {
    for i in 1..s.len() {
        if let Some(c1) = s.chars().nth(i - 1) {
            if let Some(c2) = s.chars().nth(i) {
                if let Some(c3) = s.chars().nth(i + 1) {
                    if let Some(c4) = s.chars().nth(i + 2) {
                        if c1 != c2 && c1 == c4 && c2 == c3 {
                            return true;
                        }
                    }
                }
            }
        }
    }

    false
}

fn index_all(s: &str, search: &str) -> Vec<usize> {
    let mut res = Vec::new();
    let mut i = 0;
    while i < s.len() {
        let idx = s[i..].find(search);
        match idx {
            None => return res,
            Some(t) => {
                res.push(t + i);
                i += t + search.len()
            }
        }
    }
    res
}

pub fn fn2(input: &str) -> i32 {
    let lines: Vec<_> = input.lines().collect();

    lines
        .iter()
        .filter(|s| {
            let vstart = index_all(s, "[");
            let vend = index_all(s, "]");

            let mut res = Vec::new();
            for i in 0..vstart.len() {
                let start = vstart[i];

                if start != 0 {
                    if i == 0 {
                        let mut v = abas(&s[..start]);
                        res.append(&mut v);
                    } else {
                        let mut v = abas(&s[vend[i - 1] + 1..start]);
                        res.append(&mut v);
                    }
                }
            }

            if vend[vend.len() - 1] != s.len() - 1 {
                let mut v = abas(&s[vend[vend.len() - 1] + 1..]);
                res.append(&mut v);
            }

            for i in 0..vstart.len() {
                let start = vstart[i];
                let end = vend[i];

                if bab(&s[start + 1..end], &res) {
                    return true;
                }
            }

            false
        })
        .count() as i32
}

fn abas(s: &str) -> Vec<String> {
    let mut res = Vec::new();
    for i in 0..s.len() {
        if let Some(c1) = s.chars().nth(i) {
            if let Some(c2) = s.chars().nth(i + 1) {
                if let Some(c3) = s.chars().nth(i + 2) {
                    if c1 != c2 && c1 == c3 {
                        res.push(format!("{}{}{}", c1, c2, c3));
                    }
                }
            }
        }
    }

    res
}

fn bab(s: &str, abas: &Vec<String>) -> bool {
    for aba in abas {
        let a = aba.chars().nth(0).unwrap();
        let b = aba.chars().nth(1).unwrap();
        let bab = format!("{}{}{}", b, a, b);
        if s.contains(&bab) {
            return true;
        }
    }

    false
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn test_unit() {
        assert_eq!(fn1("aaaaa[bbbb]a[xxx]abba"), 1);
    }

    #[test]
    fn test_fn1_unit() {
        let s = fs::read_to_string("test.txt").unwrap();
        assert_eq!(fn1(s.as_str()), 2);
    }

    #[test]
    fn test_fn1_input() {
        let s = fs::read_to_string("input.txt").unwrap();
        assert_eq!(fn1(s.as_str()), 69);
    }

    #[test]
    fn test_fn2_unit() {
        let s = fs::read_to_string("test.txt").unwrap();
        assert_eq!(fn2(s.as_str()), 3);
    }

    #[test]
    fn test_fn2_input() {
        let s = fs::read_to_string("input.txt").unwrap();
        assert_eq!(fn2(s.as_str()), 1);
    }
}

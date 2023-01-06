extern crate core;

pub fn fn1(s: &str, n: usize) -> i32 {
    for i in 1.. {
        let v = format!("{}{}", s, i);
        let digest = md5::compute(v);
        let res = format!("{:x}", digest);
        if starts_with_zeros(&res, n) {
            return i;
        }
    }

    -1
}

fn starts_with_zeros(s: &str, n: usize) -> bool {
    let input = std::iter::repeat("0").take(n).collect::<String>();
    s.starts_with(&input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn test_fn1_unit() {
        assert_eq!(fn1("abcdef", 5), 609043);
        assert_eq!(fn1("pqrstuv", 5), 1048970);
    }

    #[test]
    fn test_fn1_input() {
        assert_eq!(fn1("iwrupvqb", 5), 346386);
    }

    #[test]
    fn test_fn2_input() {
        assert_eq!(fn1("iwrupvqb", 6), 9958218);
    }
}

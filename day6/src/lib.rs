use std::collections::HashSet;

pub fn start_packet(input: &str, nb_chars: usize) -> usize {
    let chars: Vec<_> = input.chars().collect();
    let mut set = HashSet::new();
    let mut l = 0;
    let mut r = 0;

    loop {
        let c = chars.get(r).unwrap();
        r += 1;
        if set.contains(c) {
            while l < r {
                let c2 = chars.get(l).unwrap();
                l += 1;
                if c2 != c {
                    set.remove(c2);
                } else {
                    break;
                }
            }
        } else {
            set.insert(c);
            if r == l + nb_chars {
                break;
            }
        }
    }
    r
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn test_start_packet4_unit() {
        assert_eq!(start_packet("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 4), 7);
        assert_eq!(start_packet("bvwbjplbgvbhsrlpgdmjqwftvncz", 4), 5);
        assert_eq!(start_packet("nppdvjthqldpwncqszvftbrmjlhg", 4), 6);
        assert_eq!(start_packet("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 4), 10);
        assert_eq!(start_packet("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 4), 11);
    }

    #[test]
    fn test_start_packet4_input() {
        let s = fs::read_to_string("input.txt").unwrap();
        assert_eq!(start_packet(s.as_str(), 4), 1658);
    }

    #[test]
    fn test_start_packet14_unit() {
        assert_eq!(start_packet("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 14), 19);
        assert_eq!(start_packet("bvwbjplbgvbhsrlpgdmjqwftvncz", 14), 23);
        assert_eq!(start_packet("nppdvjthqldpwncqszvftbrmjlhg", 14), 23);
        assert_eq!(start_packet("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 14), 29);
        assert_eq!(start_packet("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 14), 26);
    }

    #[test]
    fn test_start_packet14_input() {
        let s = fs::read_to_string("input.txt").unwrap();
        assert_eq!(start_packet(s.as_str(), 14), 2260);
    }
}

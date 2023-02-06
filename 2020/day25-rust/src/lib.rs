extern crate core;

pub fn fn1(card_pk: i64, door_pk: i64) -> i64 {
    let mut card_loop_size = 0;
    let mut v: i64 = 1;
    loop {
        v = (v * 7) % 20201227;
        card_loop_size += 1;
        if v == card_pk {
            break;
        }
    }

    subject_number(door_pk, card_loop_size)
}

fn subject_number(sn: i64, lp: i64) -> i64 {
    let mut v = 1;
    for _ in 0..lp {
        v = (v * sn) % 20201227
    }
    v
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn test_fn1_unit() {
        assert_eq!(fn1(5764801, 17807724), 14897079);
    }

    #[test]
    fn test_fn1_input() {
        assert_eq!(fn1(11349501, 5107328), 7936032);
    }
}

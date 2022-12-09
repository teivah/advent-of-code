use crate::Choice::{PAPER, ROCK, SCISSORS};
use crate::Order::{DRAW, LOSE, WIN};
use std::fs::File;
use std::io::{BufRead, BufReader};

pub fn total_score_1(file: &str) -> Result<i32, Box<dyn std::error::Error>> {
    let reader = BufReader::new(File::open(file)?);

    let mut score = 0;

    for res in reader.lines() {
        let line = res?;
        let split = line.split(" ").collect::<Vec<&str>>();
        score += get_score_1(&to_choice(split[0]), &to_choice(split[1]))
    }

    Ok(score)
}

fn to_choice(choice: &str) -> Choice {
    match choice {
        "A" => ROCK,
        "B" => PAPER,
        "C" => SCISSORS,
        "X" => ROCK,
        "Y" => PAPER,
        _ => SCISSORS,
    }
}

#[derive(PartialEq)]
enum Choice {
    ROCK,
    PAPER,
    SCISSORS,
}

fn get_score_1(move1: &Choice, move2: &Choice) -> i32 {
    return win_points(move1, move2) + score_choice(move2);
}

fn score_choice(choice: &Choice) -> i32 {
    match choice {
        ROCK => 1,
        PAPER => 2,
        SCISSORS => 3,
    }
}

fn win_points(move1: &Choice, move2: &Choice) -> i32 {
    if move1 == move2 {
        return 3;
    }

    match (move1, move2) {
        (ROCK, SCISSORS) => 0,
        (ROCK, PAPER) => 6,
        (PAPER, ROCK) => 0,
        (PAPER, SCISSORS) => 6,
        (SCISSORS, PAPER) => 0,
        (SCISSORS, ROCK) => 6,
        _ => 0,
    }
}

pub fn total_score_2(file: &str) -> Result<i32, Box<dyn std::error::Error>> {
    let reader = BufReader::new(File::open(file)?);

    let mut score = 0;

    for res in reader.lines() {
        let line = res?;
        let split = line.split(" ").collect::<Vec<&str>>();
        score += get_score_2(&to_choice(split[0]), &to_order(split[1]))
    }

    Ok(score)
}

#[derive(PartialEq)]
enum Order {
    LOSE,
    DRAW,
    WIN,
}

fn to_order(choice: &str) -> Order {
    match choice {
        "X" => LOSE,
        "Y" => DRAW,
        _ => WIN,
    }
}

fn get_score_2(move1: &Choice, order: &Order) -> i32 {
    match order {
        DRAW => 3 + score_choice(move1),
        WIN => {
            6 + match move1 {
                ROCK => score_choice(&PAPER),
                PAPER => score_choice(&SCISSORS),
                SCISSORS => score_choice(&ROCK),
            }
        }
        LOSE => match move1 {
            ROCK => score_choice(&SCISSORS),
            PAPER => score_choice(&ROCK),
            SCISSORS => score_choice(&PAPER),
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn score_1_unit() {
        let result = total_score_1("test.txt");
        assert_eq!(result.unwrap(), 15);
    }

    #[test]
    fn score_1_input() {
        let result = total_score_1("input.txt");
        assert_eq!(result.unwrap(), 11386);
    }

    #[test]
    fn score_2_unit() {
        let result = total_score_2("test.txt");
        assert_eq!(result.unwrap(), 12);
    }

    #[test]
    fn score_2_input() {
        let result = total_score_2("input.txt");
        assert_eq!(result.unwrap(), 13600);
    }
}

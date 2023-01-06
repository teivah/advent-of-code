use std::fs::File;
use std::io::{BufRead, BufReader};

pub fn top1(file: &str) -> Result<i32, Box<dyn std::error::Error>> {
    let reader = BufReader::new(File::open(file)?);

    let mut current = 0;
    let mut max = 0;

    for res in reader.lines() {
        let line = res?;

        if line.eq("") {
            if current > max {
                max = current;
            }
            current = 0;
        } else {
            let n: i32 = line.to_string().parse()?;
            current += n;
        }
    }
    if current > max {
        max = current;
    }

    Ok(max)
}

pub fn top3(file: &str) -> Result<i32, Box<dyn std::error::Error>> {
    let reader = BufReader::new(File::open(file)?);

    let mut current = 0;
    let mut elves = vec![];

    for res in reader.lines() {
        let line = res?;

        if line.eq("") {
            elves.push(current);
            current = 0;
        } else {
            let n: i32 = line.to_string().parse()?;
            current += n;
        }
    }
    elves.push(current);

    elves.sort_by(|a, b| b.cmp(a));

    Ok(elves[0] + elves[1] + elves[2])
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn top1_unit() {
        let result = top1("test.txt");
        assert_eq!(result.unwrap(), 24000);
    }

    #[test]
    fn top1_input() {
        let result = top1("input.txt");
        assert_eq!(result.unwrap(), 72718);
    }

    #[test]
    fn top3_unit() {
        let result = top3("test.txt");
        assert_eq!(result.unwrap(), 45000);
    }

    #[test]
    fn top3_input() {
        let result = top3("input.txt");
        assert_eq!(result.unwrap(), 72718);
    }
}

# Advent of Code

![](https://img.shields.io/badge/stars%20⭐-400-yellow)

My solutions to the [Advents of Code](https://adventofcode.com/), from 2015 to 2023 (400 total ⭐, 2023 ongoing).

In 2023, I also shared my experience on my blog:

[<img src="post.png">](https://teivah.medium.com/advent-of-code-b5bf35a6d115)

## AoC Library

### Templates

I created a [justfile](justfile) ([casey/just](https://github.com/casey/just)) that:
* Downloads the input of the day (to authenticate, you have to set up a `ADVENT_OF_CODE_COOKIE` environment variable, see [this issue](https://github.com/wimglenn/advent-of-code-wim/issues/1) to understand how to extract your session cookie)
* Generates a skeleton project (Go, Rust, or Python)
* Configures the required dependencies (Go only)

### Go

I developed a Go library to help me solve unimportant topics as quickly as possible (e.g., puzzle input parsing) to focus on the concrete problem:

* Source: [lib branch](https://github.com/teivah/advent-of-code/tree/lib)
* [Documentation](https://pkg.go.dev/github.com/teivah/advent-of-code)

```shell
go get github.com/teivah/advent-of-code@v1.2.0
```

## Solutions

* [2023](#2023)
* [2022](#2022)
* [2021](#2021)
* [2020](#2020)
* [2019](#2019)
* [2018](#2018)
* [2017](#2017)
* [2016](#2016)
* [2015](#2015)

### 2023

* [Day 17](https://adventofcode.com/2023/day/17): [Go](2023/day17-go/main.go)
* [Day 16](https://adventofcode.com/2023/day/16): [Go](2023/day16-go/main.go)
* [Day 15](https://adventofcode.com/2023/day/15): [Go](2023/day15-go/main.go)
* [Day 14](https://adventofcode.com/2023/day/14): [Go](2023/day14-go/main.go)
* [Day 13](https://adventofcode.com/2023/day/13): [Go](2023/day13-go/main.go)
* [Day 12](https://adventofcode.com/2023/day/12): [Go](2023/day12-go/main.go)
* [Day 11](https://adventofcode.com/2023/day/11): [Go](2023/day11-go/main.go)
* [Day 10](https://adventofcode.com/2023/day/10): [Go](2023/day10-go/main.go)
* [Day 9](https://adventofcode.com/2023/day/9): [Go](2023/day9-go/main.go)
* [Day 8](https://adventofcode.com/2023/day/8): [Go](2023/day8-go/main.go)
* [Day 7](https://adventofcode.com/2023/day/7): [Go](2023/day7-go/main.go)
* [Day 6](https://adventofcode.com/2023/day/6): [Go](2023/day6-go/main.go)
* [Day 5](https://adventofcode.com/2023/day/5): [Go](2023/day5-go/main.go)
* [Day 4](https://adventofcode.com/2023/day/4): [Go](2023/day4-go/main.go)
* [Day 3](https://adventofcode.com/2023/day/3): [Go](2023/day3-go/main.go)
* [Day 2](https://adventofcode.com/2023/day/2): [Python (part 1)](2023/day2-python/main.py), [Go](2023/day2-go/main.go)
* [Day 1](https://adventofcode.com/2023/day/1): [Rust](2023/day1-rust/src/lib.rs)

### 2022

Stats: Rust (56%), Go (44%)

* [Day 25](https://adventofcode.com/2022/day/25): [Go](2022/day25-go/main.go)
* [Day 24](https://adventofcode.com/2022/day/24): [Go](2022/day24-go/main.go)
* [Day 23](https://adventofcode.com/2022/day/23): [Go](2022/day23-go/main.go)
* [Day 22](https://adventofcode.com/2022/day/22): [Go](2022/day22-go/main.go)
* [Day 21](https://adventofcode.com/2022/day/21): [Go](2022/day21-go/main.go)
* [Day 20](https://adventofcode.com/2022/day/20): [Go](2022/day20-go/main.go)
* [Day 19](https://adventofcode.com/2022/day/19): [Rust (part 1)](2022/day19-rust/src/lib.rs), [Go (part 1 and 2)](2022/day19-go/main.go)
* [Day 18](https://adventofcode.com/2022/day/16): [Rust (part 1)](2022/day18-rust/src/lib.rs), [Go (part 1 and 2)](2022/day18-go/main.go)
* [Day 17](https://adventofcode.com/2022/day/16): [Go](2022/day17-go/main.go)
* [Day 16](https://adventofcode.com/2022/day/16): [Go](2022/day16-go/main.go)
* [Day 15](https://adventofcode.com/2022/day/15): [Go](2022/day15-go/main.go)
* [Day 14](https://adventofcode.com/2022/day/14): [Rust](2022/day14-rust/src/lib.rs)
* [Day 13](https://adventofcode.com/2022/day/13): [Rust](2022/day13-rust/src/lib.rs)
* [Day 12](https://adventofcode.com/2022/day/12): [Rust](2022/day12-rust/src/lib.rs)
* [Day 11](https://adventofcode.com/2022/day/11): [Rust](2022/day11-rust/src/lib.rs)
* [Day 10](https://adventofcode.com/2022/day/10): [Rust](2022/day10-rust/src/lib.rs)
* [Day 9](https://adventofcode.com/2022/day/9): [Rust](2022/day9-rust/src/lib.rs)
* [Day 8](https://adventofcode.com/2022/day/8): [Rust](2022/day8-rust/src/lib.rs)
* [Day 7](https://adventofcode.com/2022/day/7): [Go](2022/day7-go/main.go)
* [Day 6](https://adventofcode.com/2022/day/6): [Rust](2022/day6-rust/src/lib.rs)
* [Day 5](https://adventofcode.com/2022/day/5): [Rust](2022/day5-rust/src/lib.rs)
* [Day 4](https://adventofcode.com/2022/day/4): [Rust](2022/day4-rust/src/lib.rs)
* [Day 3](https://adventofcode.com/2022/day/3): [Rust](2022/day3-rust/src/lib.rs)
* [Day 2](https://adventofcode.com/2022/day/2): [Rust](2022/day2-rust/src/lib.rs)
* [Day 1](https://adventofcode.com/2022/day/1): [Rust](2022/day1-rust/src/lib.rs)

### 2021

Stats: Go (100%)

* [Day 25](https://adventofcode.com/2021/day/25): [Go](2021/day25-go/main.go)
* [Day 24](https://adventofcode.com/2021/day/24): [Go](2021/day24-go/main.go)
* [Day 23](https://adventofcode.com/2021/day/23): [Go](2021/day23-go/main.go)
* [Day 22](https://adventofcode.com/2021/day/22): [Go](2021/day22-go/main.go)
* [Day 21](https://adventofcode.com/2021/day/21): [Go](2021/day21-go/main.go)
* [Day 20](https://adventofcode.com/2021/day/20): [Go](2021/day20-go/main.go)
* [Day 19](https://adventofcode.com/2021/day/19): [Go](2021/day19-go/main.go)
* [Day 18](https://adventofcode.com/2021/day/18): [Go](2021/day18-go/main.go)
* [Day 17](https://adventofcode.com/2021/day/17): [Go](2021/day17-go/main.go)
* [Day 16](https://adventofcode.com/2021/day/16): [Go](2021/day16-go/main.go)
* [Day 15](https://adventofcode.com/2021/day/15): [Go](2021/day15-go/main.go)
* [Day 14](https://adventofcode.com/2021/day/14): [Go](2021/day14-go/main.go)
* [Day 13](https://adventofcode.com/2021/day/13): [Go](2021/day13-go/main.go)
* [Day 12](https://adventofcode.com/2021/day/12): [Go](2021/day12-go/main.go)
* [Day 11](https://adventofcode.com/2021/day/11): [Go](2021/day11-go/main.go)
* [Day 10](https://adventofcode.com/2021/day/10): [Go](2021/day10-go/main.go)
* [Day 9](https://adventofcode.com/2021/day/9): [Go](2021/day9-go/main.go)
* [Day 8](https://adventofcode.com/2021/day/8): [Go](2021/day8-go/main.go)
* [Day 7](https://adventofcode.com/2021/day/7): [Go](2021/day7-go/main.go)
* [Day 6](https://adventofcode.com/2021/day/6): [Go](2021/day6-go/main.go)
* [Day 5](https://adventofcode.com/2021/day/5): [Go](2021/day5-go/main.go)
* [Day 4](https://adventofcode.com/2021/day/4): [Go](2021/day4-go/main.go)
* [Day 3](https://adventofcode.com/2021/day/3): [Go](2021/day3-go/main.go)
* [Day 2](https://adventofcode.com/2021/day/2): [Go](2021/day2-go/main.go)
* [Day 1](https://adventofcode.com/2021/day/1): [Go](2021/day1-go/main.go)

### 2020

Stats: Go (96%), Rust (4%)

* [Day 25](https://adventofcode.com/2020/day/25): [Rust](2020/day25-rust/src/lib.rs)
* [Day 24](https://adventofcode.com/2020/day/24): [Go](2020/day24-go/main.go)
* [Day 23](https://adventofcode.com/2020/day/23): [Go](2020/day23-go/main.go)
* [Day 22](https://adventofcode.com/2020/day/22): [Go](2020/day22-go/main.go)
* [Day 21](https://adventofcode.com/2020/day/21): [Go](2020/day21-go/main.go)
* [Day 20](https://adventofcode.com/2020/day/20): [Go](2020/day20-go/main.go)
* [Day 19](https://adventofcode.com/2020/day/19): [Go](2020/day19-go/main.go)
* [Day 18](https://adventofcode.com/2020/day/18): [Go](2020/day18-go/main.go)
* [Day 17](https://adventofcode.com/2020/day/17): [Go](2020/day17-go/main.go)
* [Day 16](https://adventofcode.com/2020/day/16): [Go](2020/day16-go/main.go)
* [Day 15](https://adventofcode.com/2020/day/15): [Go](2020/day15-go/main.go)
* [Day 14](https://adventofcode.com/2020/day/14): [Go](2020/day14-go/main.go)
* [Day 13](https://adventofcode.com/2020/day/13): [Go](2020/day13-go/main.go)
* [Day 12](https://adventofcode.com/2020/day/12): [Go](2020/day12-go/main.go)
* [Day 11](https://adventofcode.com/2020/day/11): [Go](2020/day11-go/main.go)
* [Day 10](https://adventofcode.com/2020/day/10): [Go](2020/day10-go/main.go)
* [Day 9](https://adventofcode.com/2020/day/9): [Go](2020/day9-go/main.go)
* [Day 8](https://adventofcode.com/2020/day/8): [Go](2020/day8-go/main.go)
* [Day 7](https://adventofcode.com/2020/day/7): [Go](2020/day7-go/main.go)
* [Day 6](https://adventofcode.com/2020/day/6): [Go](2020/day6-go/main.go)
* [Day 5](https://adventofcode.com/2020/day/5): [Go](2020/day5-go/main.go)
* [Day 4](https://adventofcode.com/2020/day/4): [Go](2020/day4-go/main.go)
* [Day 3](https://adventofcode.com/2020/day/3): [Go](2020/day3-go/main.go)
* [Day 2](https://adventofcode.com/2020/day/2): [Go](2020/day2-go/main.go)
* [Day 1](https://adventofcode.com/2020/day/1): [Go](2020/day1-go/main.go)

### 2019

Stats: Go (100%)

* [Day 25](https://adventofcode.com/2019/day/25): [Go](2019/day25-go/main.go)
* [Day 24](https://adventofcode.com/2019/day/24): [Go](2019/day24-go/main.go)
* [Day 23](https://adventofcode.com/2019/day/23): [Go](2019/day23-go/main.go)
* [Day 22](https://adventofcode.com/2019/day/22): [Go](2019/day22-go/main.go)
* [Day 21](https://adventofcode.com/2019/day/21): [Go](2019/day21-go/main.go)
* [Day 20](https://adventofcode.com/2019/day/20): [Go](2019/day20-go/main.go)
* [Day 19](https://adventofcode.com/2019/day/19): [Go](2019/day19-go/main.go)
* [Day 18](https://adventofcode.com/2019/day/18): [Go](2019/day18-go/main.go)
* [Day 17](https://adventofcode.com/2019/day/17): [Go](2019/day17-go/main.go)
* [Day 16](https://adventofcode.com/2019/day/16): [Go](2019/day16-go/main.go)
* [Day 15](https://adventofcode.com/2019/day/15): [Go](2019/day15-go/main.go)
* [Day 14](https://adventofcode.com/2019/day/14): [Go](2019/day14-go/main.go)
* [Day 13](https://adventofcode.com/2019/day/13): [Go](2019/day13-go/main.go)
* [Day 12](https://adventofcode.com/2019/day/12): [Go](2019/day12-go/main.go)
* [Day 11](https://adventofcode.com/2019/day/11): [Go](2019/day11-go/main.go)
* [Day 10](https://adventofcode.com/2019/day/10): [Go](2019/day10-go/main.go)
* [Day 9](https://adventofcode.com/2019/day/9): [Go](2019/day9-go/main.go)
* [Day 8](https://adventofcode.com/2019/day/8): [Go](2019/day8-go/main.go)
* [Day 7](https://adventofcode.com/2019/day/7): [Go](2019/day7-go/main.go)
* [Day 6](https://adventofcode.com/2019/day/6): [Go](2019/day6-go/main.go)
* [Day 5](https://adventofcode.com/2019/day/5): [Go](2019/day5-go/main.go)
* [Day 4](https://adventofcode.com/2019/day/4): [Go](2019/day4-go/main.go)
* [Day 3](https://adventofcode.com/2019/day/3): [Go](2019/day3-go/main.go)
* [Day 2](https://adventofcode.com/2019/day/2): [Go](2019/day2-go/main.go)
* [Day 1](https://adventofcode.com/2019/day/1): [Go](2019/day1-go/main.go)

### 2018

Stats: Go (100%)

* [Day 25](https://adventofcode.com/2018/day/25): [Go](2018/day25-go/main.go)
* [Day 24](https://adventofcode.com/2018/day/24): [Go](2018/day24-go/main.go)
* [Day 23](https://adventofcode.com/2018/day/23): [Go](2018/day23-go/main.go)
* [Day 22](https://adventofcode.com/2018/day/22): [Go](2018/day22-go/main.go)
* [Day 21](https://adventofcode.com/2018/day/21): [Go](2018/day21-go/main.go)
* [Day 20](https://adventofcode.com/2018/day/20): [Go](2018/day20-go/main.go)
* [Day 19](https://adventofcode.com/2018/day/19): [Go](2018/day19-go/main.go)
* [Day 18](https://adventofcode.com/2018/day/18): [Go](2018/day18-go/main.go)
* [Day 17](https://adventofcode.com/2018/day/17): [Go](2018/day17-go/main.go)
* [Day 16](https://adventofcode.com/2018/day/16): [Go](2018/day16-go/main.go)
* [Day 15](https://adventofcode.com/2018/day/15): [Go](2018/day15-go/main.go)
* [Day 14](https://adventofcode.com/2018/day/14): [Go](2018/day14-go/main.go)
* [Day 13](https://adventofcode.com/2018/day/13): [Go](2018/day13-go/main.go)
* [Day 12](https://adventofcode.com/2018/day/12): [Go](2018/day12-go/main.go)
* [Day 10](https://adventofcode.com/2018/day/10): [Go](2018/day10-go/main.go)
* [Day 9](https://adventofcode.com/2018/day/9): [Go](2018/day9-go/main.go)
* [Day 8](https://adventofcode.com/2018/day/8): [Go](2018/day8-go/main.go)
* [Day 7](https://adventofcode.com/2018/day/7): [Go](2018/day7-go/main.go)
* [Day 6](https://adventofcode.com/2018/day/6): [Go](2018/day6-go/main.go)
* [Day 5](https://adventofcode.com/2018/day/5): [Go](2018/day5-go/main.go)
* [Day 4](https://adventofcode.com/2018/day/4): [Go](2018/day4-go/main.go)
* [Day 3](https://adventofcode.com/2018/day/3): [Go](2018/day3-go/main.go)
* [Day 2](https://adventofcode.com/2018/day/2): [Go](2018/day2-go/main.go)
* [Day 1](https://adventofcode.com/2018/day/1): [Go](2018/day1-go/main.go)

### 2017

Stats: Go (80%), Rust (20%)

* [Day 25](https://adventofcode.com/2017/day/25): [Go](2017/day25-go/main.go)
* [Day 24](https://adventofcode.com/2017/day/24): [Go](2017/day24-go/main.go)
* [Day 23](https://adventofcode.com/2017/day/23): [Go](2017/day23-go/main.go)
* [Day 22](https://adventofcode.com/2017/day/22): [Go](2017/day22-go/main.go)
* [Day 21](https://adventofcode.com/2017/day/21): [Go](2017/day21-go/main.go)
* [Day 20](https://adventofcode.com/2017/day/20): [Go](2017/day20-go/main.go)
* [Day 19](https://adventofcode.com/2017/day/19): [Go](2017/day19-go/main.go)
* [Day 18](https://adventofcode.com/2017/day/18): [Go](2017/day18-go/main.go)
* [Day 17](https://adventofcode.com/2017/day/17): [Go](2017/day17-go/main.go)
* [Day 16](https://adventofcode.com/2017/day/16): [Go](2017/day16-go/main.go)
* [Day 15](https://adventofcode.com/2017/day/15): [Go](2017/day15-go/main.go)
* [Day 14](https://adventofcode.com/2017/day/14): [Go](2017/day14-go/main.go)
* [Day 13](https://adventofcode.com/2017/day/13): [Go](2017/day13-go/main.go)
* [Day 12](https://adventofcode.com/2017/day/12): [Go](2017/day12-go/main.go)
* [Day 11](https://adventofcode.com/2017/day/11): [Go](2017/day11-go/main.go)
* [Day 10](https://adventofcode.com/2017/day/10): [Go](2017/day10-go/main.go)
* [Day 9](https://adventofcode.com/2017/day/9): [Go](2017/day9-go/main.go)
* [Day 8](https://adventofcode.com/2017/day/8): [Go](2017/day8-go/main.go)
* [Day 7](https://adventofcode.com/2017/day/7): [Go](2017/day7-go/main.go)
* [Day 6](https://adventofcode.com/2017/day/6): [Rust](2017/day6-rust/src/lib.rs)
* [Day 5](https://adventofcode.com/2017/day/5): [Rust](2017/day5-rust/src/lib.rs)
* [Day 4](https://adventofcode.com/2017/day/4): [Go](2017/day4-go/main.go)
* [Day 3](https://adventofcode.com/2017/day/3): [Rust](2017/day3-rust/src/lib.rs)
* [Day 2](https://adventofcode.com/2017/day/2): [Rust](2017/day2-rust/src/lib.rs)
* [Day 1](https://adventofcode.com/2017/day/1): [Rust](2017/day1-rust/src/lib.rs)

### 2016

Stats: Go (76%), Rust (24%)

* [Day 25](https://adventofcode.com/2016/day/25): [Go](2016/day25-go/main.go)
* [Day 24](https://adventofcode.com/2016/day/24): [Go](2016/day24-go/main.go)
* [Day 23](https://adventofcode.com/2016/day/23): [Go](2016/day23-go/main.go)
* [Day 22](https://adventofcode.com/2016/day/22): [Go](2016/day22-go/main.go)
* [Day 21](https://adventofcode.com/2016/day/21): [Go](2016/day21-go/main.go)
* [Day 20](https://adventofcode.com/2016/day/20): [Go](2016/day20-go/main.go)
* [Day 19](https://adventofcode.com/2016/day/19): [Go](2016/day19-go/main.go)
* [Day 18](https://adventofcode.com/2016/day/18): [Go](2016/day18-go/main.go)
* [Day 17](https://adventofcode.com/2016/day/17): [Go](2016/day17-go/main.go)
* [Day 16](https://adventofcode.com/2016/day/16): [Go](2016/day16-go/main.go)
* [Day 15](https://adventofcode.com/2016/day/15): [Go](2016/day15-go/main.go)
* [Day 14](https://adventofcode.com/2016/day/14): [Rust](2016/day14-rust/src/lib.rs)
* [Day 13](https://adventofcode.com/2016/day/13): [Rust](2016/day13-rust/src/lib.rs)
* [Day 12](https://adventofcode.com/2016/day/12): [Go](2016/day12-go/main.go)
* [Day 11](https://adventofcode.com/2016/day/11): [Go](2016/day11-go/main.go)
* [Day 10](https://adventofcode.com/2016/day/10): [Go](2016/day10-go/main.go)
* [Day 9](https://adventofcode.com/2016/day/9): [Go](2016/day9-go/main.go)
* [Day 8](https://adventofcode.com/2016/day/8): [Go](2016/day8-go/main.go)
* [Day 7](https://adventofcode.com/2016/day/7): [Rust](2016/day7-rust/src/lib.rs)
* [Day 6](https://adventofcode.com/2016/day/6): [Rust](2016/day6-rust/src/lib.rs)
* [Day 5](https://adventofcode.com/2016/day/5): [Rust](2016/day5-rust/src/lib.rs)
* [Day 4](https://adventofcode.com/2016/day/4): [Go](2016/day4-go/main.go)
* [Day 3](https://adventofcode.com/2016/day/3): [Go](2016/day3-go/main.go)
* [Day 2](https://adventofcode.com/2016/day/2): [Go](2016/day2-go/main.go)
* [Day 1](https://adventofcode.com/2016/day/1): [Rust](2016/day1-rust/src/lib.rs)

### 2015

Stats: Go (68%), Rust (32%)

* [Day 25](https://adventofcode.com/2015/day/25): [Go](2015/day25-go/main.go)
* [Day 24](https://adventofcode.com/2015/day/24): [Go](2015/day24-go/main.go)
* [Day 23](https://adventofcode.com/2015/day/23): [Go](2015/day23-go/main.go)
* [Day 22](https://adventofcode.com/2015/day/22): [Go](2015/day22-go/main.go)
* [Day 21](https://adventofcode.com/2015/day/21): [Go](2015/day21-go/main.go)
* [Day 20](https://adventofcode.com/2015/day/20): [Go](2015/day20-go/main.go)
* [Day 19](https://adventofcode.com/2015/day/19): [Go](2015/day19-go/main.go)
* [Day 18](https://adventofcode.com/2015/day/18): [Go](2015/day18-go/main.go)
* [Day 17](https://adventofcode.com/2015/day/17): [Go](2015/day17-go/main.go)
* [Day 16](https://adventofcode.com/2015/day/16): [Rust](2015/day16-rust/src/lib.rs)
* [Day 15](https://adventofcode.com/2015/day/15): [Go](2015/day15-go/main.go)
* [Day 14](https://adventofcode.com/2015/day/14): [Go](2015/day14-go/main.go)
* [Day 13](https://adventofcode.com/2015/day/13): [Go](2015/day13-go/main.go)
* [Day 12](https://adventofcode.com/2015/day/12): [Go](2015/day12-go/main.go)
* [Day 11](https://adventofcode.com/2015/day/11): [Rust](2015/day11-rust/src/lib.rs)
* [Day 10](https://adventofcode.com/2015/day/10): [Go](2015/day10-go/main.go)
* [Day 9](https://adventofcode.com/2015/day/9): [Go](2015/day9-go/main.go)
* [Day 8](https://adventofcode.com/2015/day/8): [Go](2015/day8-go/main.go)
* [Day 7](https://adventofcode.com/2015/day/7): [Go](2015/day7-go/main.go)
* [Day 6](https://adventofcode.com/2015/day/6): [Rust](2015/day6-rust/src/lib.rs)
* [Day 5](https://adventofcode.com/2015/day/5): [Rust](2015/day5-rust/src/lib.rs)
* [Day 4](https://adventofcode.com/2015/day/4): [Rust](2015/day4-rust/src/lib.rs)
* [Day 3](https://adventofcode.com/2015/day/3): [Rust](2015/day3-rust/src/lib.rs)
* [Day 2](https://adventofcode.com/2015/day/2): [Rust](2015/day2-rust/src/lib.rs)
* [Day 1](https://adventofcode.com/2015/day/1): [Rust](2015/day1-rust/src/lib.rs)

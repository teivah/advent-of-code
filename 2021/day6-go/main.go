package main

import (
	"io"
	"strconv"

	aoc "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader, days int) int {
	ages := aoc.NewDelimiter(aoc.ReaderToString(input), ",").GetInts()
	fishes := make([]*Fish, len(ages))
	for i, age := range ages {
		fishes[i] = &Fish{
			current: age,
		}
	}

	for day := 0; day < days; day++ {
		l := len(fishes)
		for i := 0; i < l; i++ {
			fish := fishes[i]
			fish.current--
			if fish.current < 0 {
				fishes = append(fishes, &Fish{
					current: 8,
				})
				fish.current = 6
			}
		}
	}

	return len(fishes)
}

type Fish struct {
	current int
}

func (f Fish) String() string {
	return strconv.Itoa(f.current)
}

func fs2(input io.Reader, days int) int {
	ages := aoc.NewDelimiter(aoc.ReaderToString(input), ",").GetInts()
	m := make(map[int]int)
	for _, age := range ages {
		m[age]++
	}

	for day := 0; day < days; day++ {
		res := make(map[int]int)
		for age, count := range m {
			age--
			if age < 0 {
				res[6] += count
				res[8] += count
			} else {
				res[age] += count
			}
		}
		m = res
	}

	sum := 0
	for _, v := range m {
		sum += v
	}
	return sum
}

package main

import (
	"io"
	"strconv"

	"github.com/teivah/go-aoc"
)

func fs(input io.Reader, count int) int {
	digits := aoc.NewDelimiter(aoc.ReaderToString(input), " ").GetInts()
	res := 0
	s := solver{dp: map[aoc.Pair[int, int]]int{}}
	for _, digit := range digits {
		res += s.solve(digit, count)
	}
	return res
}

type solver struct {
	dp map[aoc.Pair[int, int]]int
}

func (s solver) solve(n int, count int) (res int) {
	if count == 0 {
		return 1
	}
	k := aoc.Pair[int, int]{n, count}
	if v, ok := s.dp[k]; ok {
		return v
	}
	defer func() {
		s.dp[k] = res
	}()

	digits := strconv.Itoa(n)
	switch {
	case n == 0:
		return s.solve(1, count-1)
	case len(digits)%2 == 0:
		return s.solve(aoc.StringToInt(digits[:len(digits)/2]), count-1) +
			s.solve(aoc.StringToInt(digits[len(digits)/2:]), count-1)
	default:
		return s.solve(n*2024, count-1)
	}
}

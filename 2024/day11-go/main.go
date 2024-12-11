package main

import (
	"fmt"
	"io"

	"github.com/teivah/go-aoc"
)

func fs(input io.Reader, count int) int {
	digits := aoc.NewDelimiter(aoc.ReaderToString(input), " ").GetInts()
	res := 0
	s := solver{dp: map[key]int{}}
	for _, digit := range digits {
		res += s.solve(digit, count)
	}
	return res
}

type key struct {
	n     int
	count int
}

type solver struct {
	dp map[key]int
}

func (s solver) solve(n int, count int) int {
	if count == 0 {
		return 1
	}
	if v, ok := s.dp[key{n, count}]; ok {
		return v
	}

	digits := fmt.Sprintf("%d", n)
	k := key{n, count}
	res := 0
	switch {
	case n == 0:
		res = s.solve(1, count-1)
	case len(digits)%2 == 0:
		res = s.solve(aoc.StringToInt(digits[:len(digits)/2]), count-1) +
			s.solve(aoc.StringToInt(digits[len(digits)/2:]), count-1)
	default:
		res = s.solve(n*2024, count-1)
	}
	s.dp[k] = res
	return res
}

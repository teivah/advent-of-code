package main

import (
	"io"
	"sort"

	aoc "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	s := aoc.ReaderToString(input)
	positions := aoc.NewDelimiter(s, ",").GetInts()
	sort.Ints(positions)
	median := 0
	if len(positions)%2 == 0 {
		median = positions[len(positions)/2-1]
	} else {
		median = positions[len(positions)/2]
	}

	return positionFromMedian(positions, median)
}

func positionFromMedian(positions []int, median int) int {
	sum := 0
	for _, position := range positions {
		sum += aoc.Abs(median - position)
	}
	return sum
}

func fs2(input io.Reader) int {
	s := aoc.ReaderToString(input)
	positions := aoc.NewDelimiter(s, ",").GetInts()

	minMax := aoc.NewMinerMaxer()
	for _, position := range positions {
		minMax.Add(position)
	}

	m := aoc.NewMiner()
	for i := minMax.GetMin(); i <= minMax.GetMax(); i++ {
		v := totalDistance(positions, i)
		m.Add(v)
	}

	return m.Get()
}

func totalDistance(positions []int, target int) int {
	sum := 0
	for _, position := range positions {
		sum += distance(aoc.Abs(position - target))
	}
	return sum
}

var dp = map[int]int{}

func distance(d int) int {
	if d == 0 {
		return 0
	}
	if v, exists := dp[d]; exists {
		return v
	}
	v := d + distance(d-1)
	dp[d] = v
	return v
}

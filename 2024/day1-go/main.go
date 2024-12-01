package main

import (
	"io"
	"sort"

	aoc "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	lines := aoc.ReaderToStrings(input)

	a1 := make([]int, 0, len(lines))
	a2 := make([]int, 0, len(lines))
	for _, line := range lines {
		del := aoc.NewDelimiter(line, " ")
		ints := del.GetInts()
		a1 = append(a1, ints[0])
		a2 = append(a2, ints[1])
	}

	sort.Ints(a1)
	sort.Ints(a2)

	res := 0
	for i := range a1 {
		res += aoc.Abs(a1[i] - a2[i])
	}

	return res
}

func fs2(input io.Reader) int {
	lines := aoc.ReaderToStrings(input)

	a1 := make([]int, 0, len(lines))
	a2 := make([]int, 0, len(lines))
	for _, line := range lines {
		del := aoc.NewDelimiter(line, " ")
		ints := del.GetInts()
		a1 = append(a1, ints[0])
		a2 = append(a2, ints[1])
	}

	count := make(map[int]int)
	for _, v := range a2 {
		count[v]++
	}

	res := 0
	for _, v := range a1 {
		res += v * count[v]
	}
	return res
}

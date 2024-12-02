package main

import (
	"io"

	aoc "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	lines := aoc.ReaderToStrings(input)
	res := 0
	for _, line := range lines {
		del := aoc.NewDelimiter(line, " ")
		if isSafe(del.GetInts()) {
			res++
		}
	}
	return res
}

func isSafe(elems []int) bool {
	prev := elems[0]
	increase := true
	if elems[1] == elems[0] {
		return false
	}
	if elems[1] < elems[0] {
		increase = false
	}
	for i := 1; i < len(elems); i++ {
		cur := elems[i]
		if increase && cur < prev {
			return false
		}
		if !increase && cur > prev {
			return false
		}
		diff := aoc.Abs(prev - cur)
		if diff < 1 || diff > 3 {
			return false
		}
		prev = cur
	}
	return true
}

func fs2(input io.Reader) int {
	lines := aoc.ReaderToStrings(input)
	res := 0
	for _, line := range lines {
		del := aoc.NewDelimiter(line, " ")
		elems := del.GetInts()

		if isSafe2(elems) {
			res++
		}
	}
	return res
}

func isSafe2(elems []int) bool {
	if isSafe(elems) {
		return true
	}
	for i := 0; i < len(elems); i++ {
		e := aoc.FilterSliceIndices(elems, []int{i})
		if isSafe(e) {
			return true
		}
	}
	return false
}

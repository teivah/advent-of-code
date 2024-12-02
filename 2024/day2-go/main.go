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
		e := remove(elems, i)
		if isSafe(e) {
			return true
		}
	}
	return false
}

func remove(elems []int, i int) []int {
	l := len(elems)
	if i == 0 {
		return elems[1:]
	}
	if i == l-1 {
		return elems[0 : l-1]
	}
	e := make([]int, len(elems))
	copy(e, elems)
	return append(e[0:i], e[i+1:]...)
}

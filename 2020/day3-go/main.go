package main

import (
	"io"

	lib "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	lines := lib.ReaderToStrings(input)
	return count(lines, 1, 3)
}

func count(lines []string, deltaRow, deltaCol int) int {
	col := 0
	sum := 0
	for row := 1; row < len(lines); row += deltaRow {
		col = (col + deltaCol) % len(lines[0])
		if lines[row][col] == '#' {
			sum++
		}
	}

	return sum
}

func fs2(input io.Reader) int {
	lines := lib.ReaderToStrings(input)
	return count(lines, 1, 1) *
		count(lines, 1, 3) *
		count(lines, 1, 5) *
		count(lines, 1, 7) *
		count(lines, 2, 1)
}

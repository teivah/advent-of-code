package main

import (
	"io"

	aoc "github.com/teivah/advent-of-code"
)

func fs(input io.Reader, expansion int) int {
	lines := aoc.ReaderToStrings(input)
	positions, rowsDistance, colsDistance := parse(lines, expansion)

	res := 0
	for i := 0; i < len(positions); i++ {
		for j := i + 1; j < len(positions); j++ {
			pos1 := positions[i]
			pos2 := positions[j]
			res += aoc.Abs(rowsDistance[pos1.Row]-rowsDistance[pos2.Row]) +
				aoc.Abs(colsDistance[pos1.Col]-colsDistance[pos2.Col])
		}
	}
	return res
}

func parse(lines []string, expansion int) ([]aoc.Position, []int, []int) {
	var positions []aoc.Position
	rowsDistance := make([]int, len(lines))
	sum := 0
	for row, line := range lines {
		runes := []rune(line)
		empty := true
		for col, r := range runes {
			if r == '#' {
				positions = append(positions, aoc.Position{Row: row, Col: col})
				empty = false
			}
		}

		rowsDistance[row] = sum
		if empty {
			sum += expansion
		} else {
			sum++
		}
	}

	colsDistance := make([]int, len(lines[0]))
	sum = 0
	for col := 0; col < len(lines[0]); col++ {
		empty := true
		for row := 0; row < len(lines); row++ {
			if lines[row][col] == '#' {
				empty = false
				break
			}
		}

		colsDistance[col] = sum
		if empty {
			sum += expansion
		} else {
			sum++
		}
	}

	return positions, rowsDistance, colsDistance
}

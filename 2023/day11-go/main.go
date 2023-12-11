package main

import (
	"io"

	aoc "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	lines := aoc.ReaderToStrings(input)
	board, rows, cols := parse(lines)

	var positions []aoc.Position
	for pos := range board {
		positions = append(positions, pos)
	}

	res := 0
	for i := 0; i < len(positions); i++ {
		for j := i + 1; j < len(positions); j++ {
			res += getDistance(rows, cols, positions[i], positions[j], 1)
		}
	}
	return res
}

func parse(lines []string) (map[aoc.Position]bool, map[int]bool, map[int]bool) {
	m := make(map[aoc.Position]bool)
	rows := make(map[int]bool)
	for row, line := range lines {
		runes := []rune(line)
		empty := true
		for col, r := range runes {
			if r == '#' {
				m[aoc.Position{Row: row, Col: col}] = true
				empty = false
			}
		}
		if empty {
			rows[row] = true
		}
	}

	cols := make(map[int]bool)
	for col := 0; col < len(lines[0]); col++ {
		empty := true
		for row := 0; row < len(lines); row++ {
			if lines[row][col] == '#' {
				empty = false
				break
			}
		}
		if empty {
			cols[col] = true
		}
	}

	return m, rows, cols
}

func getDistance(rows map[int]bool, cols map[int]bool, pos1, pos2 aoc.Position, empty int) int {
	d := pos1.Manhattan(pos2)

	if pos1.Row < pos2.Row {
		for row := pos1.Row; row < pos2.Row; row++ {
			if rows[row] {
				d += empty
			}
		}
	} else if pos1.Row > pos2.Row {
		for row := pos1.Row - 1; row > pos2.Row; row-- {
			if rows[row] {
				d += empty
			}
		}
	}

	if pos1.Col < pos2.Col {
		for Col := pos1.Col; Col < pos2.Col; Col++ {
			if cols[Col] {
				d += empty
			}
		}
	} else if pos1.Col > pos2.Col {
		for Col := pos1.Col - 1; Col > pos2.Col; Col-- {
			if cols[Col] {
				d += empty
			}
		}
	}

	return d
}

func fs2(input io.Reader, empty int) int {
	lines := aoc.ReaderToStrings(input)
	board, rows, cols := parse(lines)

	var positions []aoc.Position
	for pos := range board {
		positions = append(positions, pos)
	}

	res := 0
	for i := 0; i < len(positions); i++ {
		for j := i + 1; j < len(positions); j++ {
			res += getDistance(rows, cols, positions[i], positions[j], empty-1)
		}
	}
	return res
}

package main

import (
	"fmt"
	"io"
	"strings"

	aoc "github.com/teivah/advent-of-code"
)

type cellType int

const (
	empty cellType = iota
	roundRock
	cubeRock
)

func fs1(input io.Reader) int {
	lines := aoc.ReaderToStrings(input)
	board, rows, cols := parse(lines)

	for row := 1; row < rows; row++ {
		for col := 0; col < cols; col++ {
			move(aoc.Position{Row: row, Col: col}, board, aoc.Up)
		}
	}
	return getResult(board, rows)
}

func parse(lines []string) (map[aoc.Position]cellType, int, int) {
	m := make(map[aoc.Position]cellType)
	for row, line := range lines {
		for col, c := range line {
			pos := aoc.Position{Row: row, Col: col}
			switch c {
			case '.':
				m[pos] = empty
			case 'O':
				m[pos] = roundRock
			case '#':
				m[pos] = cubeRock
			default:
				panic(c)
			}
		}
	}
	return m, len(lines), len(lines[0])
}

func move(pos aoc.Position, board map[aoc.Position]cellType, dir aoc.Direction) {
	if board[pos] != roundRock {
		return
	}

	next := pos.Move(dir, 1)
	t, exists := board[next]
	if !exists || t != empty {
		return
	}

	board[pos], board[next] = board[next], board[pos]
	move(next, board, dir)
}

func getResult(board map[aoc.Position]cellType, rows int) int {
	res := 0
	for pos, t := range board {
		if t == roundRock {
			res += rows - pos.Row
		}
	}
	return res
}

func fs2(input io.Reader, count int) int {
	lines := aoc.ReaderToStrings(input)
	board, rows, cols := parse(lines)

	existingPositions := make(map[string]int)
	for i := 0; i < count; i++ {
		// North
		for row := 1; row < rows; row++ {
			for col := 0; col < cols; col++ {
				move(aoc.Position{Row: row, Col: col}, board, aoc.Up)
			}
		}

		// West (left)
		for col := 1; col < cols; col++ {
			for row := 0; row < rows; row++ {
				move(aoc.Position{Row: row, Col: col}, board, aoc.Left)
			}
		}

		// South
		for row := rows - 2; row >= 0; row-- {
			for col := 0; col < cols; col++ {
				move(aoc.Position{Row: row, Col: col}, board, aoc.Down)
			}
		}

		// East (right)
		for col := cols - 2; col >= 0; col-- {
			for row := 0; row < rows; row++ {
				move(aoc.Position{Row: row, Col: col}, board, aoc.Right)
			}
		}

		h := hashPosition(board, rows, cols)
		if v, exists := existingPositions[h]; exists {
			delta := i - v
			// Speed up the computation by jumping close to count.
			for ; i < (count - delta); i += delta {
			}
		} else {
			existingPositions[h] = i
		}
	}

	return getResult(board, rows)
}

func hashPosition(board map[aoc.Position]cellType, rows int, cols int) string {
	var res []string
	for row := 0; row < rows; row++ {
		for col := 0; col < cols; col++ {
			pos := aoc.Position{Row: row, Col: col}
			if board[pos] == roundRock {
				res = append(res, fmt.Sprintf("%v", pos))
			}
		}
	}
	return strings.Join(res, ";")
}

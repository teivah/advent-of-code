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
	round
	cube
)

func fs1(input io.Reader) int {
	lines := aoc.ReaderToStrings(input)
	board, rows, cols := parse(lines)

	for row := 1; row < rows; row++ {
		for col := 0; col < cols; col++ {
			pos := aoc.Position{Row: row, Col: col}
			if board[pos] == round {
				move(pos, board, aoc.Up)
			}
		}
	}

	res := 0
	for pos, t := range board {
		if t == round {
			res += rows - pos.Row
		}
	}
	return res
}

func move(pos aoc.Position, board map[aoc.Position]cellType, dir aoc.Direction) {
	// Invariant check
	if board[pos] != round {
		panic(pos)
	}

	next := pos.Move(dir, 1)

	v, exists := board[next]
	if !exists {
		return
	}

	if v != empty {
		return
	}
	board[pos] = empty
	board[next] = round
	move(next, board, dir)
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
				m[pos] = round
			case '#':
				m[pos] = cube
			default:
				panic(c)
			}
		}
	}
	return m, len(lines), len(lines[0])
}

func fs2(input io.Reader, count int) int {
	lines := aoc.ReaderToStrings(input)
	board, rows, cols := parse(lines)

	cache := make(map[string]int)
	cache[hashPosition(board, rows, cols)] = 0
	deltaFound := false

	for i := 0; i < count; i++ {
		// North
		for row := 1; row < rows; row++ {
			for col := 0; col < cols; col++ {
				pos := aoc.Position{Row: row, Col: col}
				if board[pos] == round {
					move(pos, board, aoc.Up)
				}
			}
		}

		// West (left)
		for col := 1; col < cols; col++ {
			for row := 0; row < rows; row++ {
				pos := aoc.Position{Row: row, Col: col}
				if board[pos] == round {
					move(pos, board, aoc.Left)
				}
			}
		}

		// South
		for row := rows - 1; row >= 0; row-- {
			for col := 0; col < cols; col++ {
				pos := aoc.Position{Row: row, Col: col}
				if board[pos] == round {
					move(pos, board, aoc.Down)
				}
			}
		}

		// East (right)
		for col := cols - 1; col >= 0; col-- {
			for row := 0; row < rows; row++ {
				pos := aoc.Position{Row: row, Col: col}
				if board[pos] == round {
					move(pos, board, aoc.Right)
				}
			}
		}

		//fmt.Printf("Iteration %d\n", i+1)
		//printBoard(board, rows, cols)

		if !deltaFound {
			h := hashPosition(board, rows, cols)
			if v, exists := cache[h]; exists {
				fmt.Printf("last seen %d, now %d, delta %d\n", v, i, i-v)
				delta := i - v
				for ; i < count; i += delta {

				}
				i -= delta
				deltaFound = true
				continue
			}
			cache[h] = i
		}
	}

	res := 0
	for pos, t := range board {
		if t == round {
			res += rows - pos.Row
		}
	}
	return res
}

func printBoard(board map[aoc.Position]cellType, rows int, cols int) {
	for row := 0; row < rows; row++ {
		for col := 0; col < cols; col++ {
			t := board[aoc.Position{Row: row, Col: col}]
			switch t {
			case empty:
				fmt.Print(".")
			case round:
				fmt.Print("O")
			case cube:
				fmt.Print("#")
			}
		}
		fmt.Println()
	}
	fmt.Println()
}

func hashPosition(board map[aoc.Position]cellType, rows int, cols int) string {
	var res []string
	for row := 0; row < rows; row++ {
		for col := 0; col < cols; col++ {
			pos := aoc.Position{Row: row, Col: col}
			if board[pos] == round {
				res = append(res, fmt.Sprintf("%v", pos))
			}
		}
	}
	return strings.Join(res, ",")
}

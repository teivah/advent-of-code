package main

import (
	"fmt"
	"io"

	aoc "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	grid := toGrid(aoc.ReaderToStrings(input))

	before := fmt.Sprintf("%s", grid)
	for {
		grid.round()
		after := fmt.Sprintf("%s", grid)
		if before == after {
			return grid.occupied()
		}
		before = after
	}
}

type Grid struct {
	row   int
	col   int
	board [][]Unit
}

func (g *Grid) String() string {
	s := ""
	for row := 0; row < g.row; row++ {
		for col := 0; col < g.col; col++ {
			pos := aoc.Position{row, col}
			s += fmt.Sprintf("%c", g.seatType(pos))
		}
		s += "\n"
	}
	return s
}

func toGrid(lines []string) *Grid {
	board := make([][]Unit, len(lines))
	for i, line := range lines {
		board[i] = make([]Unit, len(line))
		for j := 0; j < len(line); j++ {
			switch line[j] {
			case 'L':
				board[i][j] = empty
			case '#':
				board[i][j] = occupied
			case '.':
				board[i][j] = floor
			}
		}
	}
	return &Grid{
		row:   len(lines),
		col:   len(lines[0]),
		board: board,
	}
}

func (g *Grid) newBoard() [][]Unit {
	board := make([][]Unit, g.row)
	for i := 0; i < g.row; i++ {
		board[i] = make([]Unit, g.col)
	}
	return board
}

func (g *Grid) seatType(pos aoc.Position) Unit {
	return g.board[pos.Row][pos.Col]
}

func (g *Grid) isOccupied(pos aoc.Position) int {
	if pos.Row < 0 || pos.Row >= g.row || pos.Col < 0 || pos.Col >= g.col {
		return 0
	}

	if g.board[pos.Row][pos.Col] == occupied {
		return 1
	}
	return 0
}

func (g *Grid) round() {
	board := g.newBoard()
	for row := 0; row < g.row; row++ {
		for col := 0; col < g.col; col++ {
			pos := aoc.Position{row, col}
			sum := g.isOccupied(pos.Delta(-1, -1)) +
				g.isOccupied(pos.Delta(-1, 0)) +
				g.isOccupied(pos.Delta(-1, 1)) +
				g.isOccupied(pos.Delta(0, -1)) +
				g.isOccupied(pos.Delta(0, 1)) +
				g.isOccupied(pos.Delta(1, -1)) +
				g.isOccupied(pos.Delta(1, 0)) +
				g.isOccupied(pos.Delta(1, 1))

			seatType := g.seatType(pos)
			if seatType == empty && sum == 0 {
				board[row][col] = occupied
				continue
			}
			if seatType == occupied && sum >= 4 {
				board[row][col] = empty
				continue
			}
			board[row][col] = seatType
		}
	}
	g.board = board
}

func (g *Grid) occupied() int {
	sum := 0
	for _, row := range g.board {
		for _, unit := range row {
			if unit == occupied {
				sum++
			}
		}
	}
	return sum
}

type Unit rune

const (
	empty    Unit = 'L'
	occupied Unit = '#'
	floor    Unit = '.'
)

func fs2(input io.Reader) int {
	grid := toGrid(aoc.ReaderToStrings(input))

	before := fmt.Sprintf("%s", grid)
	for {
		grid.round2()
		after := fmt.Sprintf("%s", grid)
		if before == after {
			return grid.occupied()
		}
		before = after
	}
}

func (g *Grid) round2() {
	board := g.newBoard()
	for row := 0; row < g.row; row++ {
		for col := 0; col < g.col; col++ {
			pos := aoc.Position{row, col}
			sum := g.isOccupiedRange(pos.Delta(-1, -1), aoc.UpLeft) +
				g.isOccupiedRange(pos.Delta(-1, 0), aoc.Up) +
				g.isOccupiedRange(pos.Delta(-1, 1), aoc.UpRight) +
				g.isOccupiedRange(pos.Delta(0, -1), aoc.Left) +
				g.isOccupiedRange(pos.Delta(0, 1), aoc.Right) +
				g.isOccupiedRange(pos.Delta(1, -1), aoc.DownLeft) +
				g.isOccupiedRange(pos.Delta(1, 0), aoc.Down) +
				g.isOccupiedRange(pos.Delta(1, 1), aoc.DownRight)

			seatType := g.seatType(pos)
			if seatType == empty && sum == 0 {
				board[row][col] = occupied
				continue
			}
			if seatType == occupied && sum >= 5 {
				board[row][col] = empty
				continue
			}
			board[row][col] = seatType
		}
	}
	g.board = board
}

func (g *Grid) isOccupiedRange(pos aoc.Position, direction aoc.Direction) int {
	if pos.Row < 0 || pos.Row >= g.row || pos.Col < 0 || pos.Col >= g.col {
		return 0
	}

	if g.board[pos.Row][pos.Col] == occupied {
		return 1
	}

	if g.board[pos.Row][pos.Col] == empty {
		return 0
	}

	return g.isOccupiedRange(pos.Move(direction, 1), direction)
}

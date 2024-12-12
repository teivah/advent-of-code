package main

import (
	"io"

	"github.com/teivah/go-aoc"
)

func fs1(input io.Reader) int {
	board := aoc.NewBoardFromReader(input, func(row, col int, r rune) rune {
		return r
	})
	visited := make(map[aoc.Position]bool)
	res := 0
	for pos, r := range board.Positions {
		shape := make(map[aoc.Position]bool)
		n := perimeter(r, board, pos, visited, shape)
		res += len(shape) * n
	}
	return res
}

func fs2(input io.Reader) int {
	board := aoc.NewBoardFromReader(input, func(row, col int, r rune) rune {
		return r
	})
	visited := make(map[aoc.Position]bool)
	res := 0
	for pos, r := range board.Positions {
		shape := make(map[aoc.Position]bool)
		before := len(visited)
		_ = perimeter(r, board, pos, visited, shape)
		after := len(visited)
		if after == before {
			continue
		}
		count := countSides(board, shape)
		res += (after - before) * count
	}
	return res
}

func perimeter(r rune, board aoc.Board[rune], pos aoc.Position, visited, shape map[aoc.Position]bool) int {
	if !board.Contains(pos) {
		return 1
	}
	if visited[pos] {
		if board.Get(pos) == r {
			return 0
		}
		return 1
	}
	if r != board.Get(pos) {
		return 1
	}
	visited[pos] = true
	shape[pos] = true
	return perimeter(r, board, pos.Move(aoc.Up, 1), visited, shape) +
		perimeter(r, board, pos.Move(aoc.Left, 1), visited, shape) +
		perimeter(r, board, pos.Move(aoc.Right, 1), visited, shape) +
		perimeter(r, board, pos.Move(aoc.Down, 1), visited, shape)
}

func countSides(board aoc.Board[rune], internal map[aoc.Position]bool) int {
	count := 0

	// From left
	for col := 0; col < board.MaxCols; col++ {
		for row := 0; row < board.MaxRows; row++ {
			if internal[aoc.NewPosition(row, col)] && !internal[aoc.NewPosition(row, col-1)] {
				count++
				row++
				for ; row < board.MaxRows; row++ {
					if !(internal[aoc.NewPosition(row, col)] && !internal[aoc.NewPosition(row, col-1)]) {
						break
					}
				}
			}
		}
	}

	// From right
	for col := board.MaxCols - 1; col >= 0; col-- {
		for row := 0; row < board.MaxRows; row++ {
			if internal[aoc.NewPosition(row, col)] && !internal[aoc.NewPosition(row, col+1)] {
				count++
				row++
				for ; row < board.MaxRows; row++ {
					if !(internal[aoc.NewPosition(row, col)] && !internal[aoc.NewPosition(row, col+1)]) {
						break
					}
				}
			}
		}
	}

	// From top
	for row := 0; row < board.MaxRows; row++ {
		for col := 0; col < board.MaxCols; col++ {
			if internal[aoc.NewPosition(row, col)] && !internal[aoc.NewPosition(row-1, col)] {
				count++
				col++
				for ; col < board.MaxCols; col++ {
					if !(internal[aoc.NewPosition(row, col)] && !internal[aoc.NewPosition(row-1, col)]) {
						break
					}
				}
			}
		}
	}

	// From bottom
	for row := board.MaxRows - 1; row >= 0; row-- {
		for col := 0; col < board.MaxCols; col++ {
			if internal[aoc.NewPosition(row, col)] && !internal[aoc.NewPosition(row+1, col)] {
				count++
				col++
				for ; col < board.MaxCols; col++ {
					if !(internal[aoc.NewPosition(row, col)] && !internal[aoc.NewPosition(row+1, col)]) {
						break
					}
				}
			}
		}
	}

	return count
}

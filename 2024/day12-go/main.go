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
		before := len(visited)
		n := perimeter(r, board, pos, visited)
		after := len(visited)
		res += (after - before) * n
	}
	return res
}

func perimeter(r rune, board aoc.Board[rune], pos aoc.Position, visited map[aoc.Position]bool) int {
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
	return perimeter(r, board, pos.Move(aoc.Up, 1), visited) +
		perimeter(r, board, pos.Move(aoc.Left, 1), visited) +
		perimeter(r, board, pos.Move(aoc.Right, 1), visited) +
		perimeter(r, board, pos.Move(aoc.Down, 1), visited)
}

func fs2(input io.Reader) int {
	board := aoc.NewBoardFromReader(input, func(row, col int, r rune) rune {
		return r
	})
	visited := make(map[aoc.Position]bool)
	res := 0
	for pos, r := range board.Positions {
		internal := make(map[aoc.Position]bool)
		external := make(map[aoc.Position]bool)
		before := len(visited)
		sides(r, board, pos, visited, internal, external)
		after := len(visited)
		if after == before {
			continue
		}
		count := countSides(board, internal)
		res += (after - before) * count
	}
	return res
}

func countSides(board aoc.Board[rune], internal map[aoc.Position]bool) int {
	count := 0

	// From left
	for col := 0; col < board.MaxCols; col++ {
		for row := 0; row < board.MaxRows; row++ {
			cur := aoc.NewPosition(row, col)
			prev := aoc.NewPosition(row, col-1)
			if internal[cur] && !internal[prev] {
				count++
				row++
				for ; row < board.MaxRows; row++ {
					cur := aoc.NewPosition(row, col)
					prev := aoc.NewPosition(row, col-1)
					if !(internal[cur] && !internal[prev]) {
						break
					}
				}
			}
		}
	}

	// From right
	for col := board.MaxCols - 1; col >= 0; col-- {
		for row := 0; row < board.MaxRows; row++ {
			cur := aoc.NewPosition(row, col)
			prev := aoc.NewPosition(row, col+1)
			if internal[cur] && !internal[prev] {
				count++
				row++
				for ; row < board.MaxRows; row++ {
					cur := aoc.NewPosition(row, col)
					prev := aoc.NewPosition(row, col+1)
					if !(internal[cur] && !internal[prev]) {
						break
					}
				}
			}
		}
	}

	// From top
	for row := 0; row < board.MaxRows; row++ {
		for col := 0; col < board.MaxCols; col++ {
			cur := aoc.NewPosition(row, col)
			prev := aoc.NewPosition(row-1, col)
			if internal[cur] && !internal[prev] {
				count++
				col++
				for ; col < board.MaxCols; col++ {
					cur := aoc.NewPosition(row, col)
					prev := aoc.NewPosition(row-1, col)
					if !(internal[cur] && !internal[prev]) {
						break
					}
				}
			}
		}
	}

	// From bottom
	for row := board.MaxRows - 1; row >= 0; row-- {
		for col := 0; col < board.MaxCols; col++ {
			cur := aoc.NewPosition(row, col)
			prev := aoc.NewPosition(row+1, col)
			if internal[cur] && !internal[prev] {
				count++
				col++
				for ; col < board.MaxCols; col++ {
					cur := aoc.NewPosition(row, col)
					prev := aoc.NewPosition(row+1, col)
					if !(internal[cur] && !internal[prev]) {
						break
					}
				}
			}
		}
	}

	return count
}

func sides(r rune, board aoc.Board[rune], pos aoc.Position, visited, internal, external map[aoc.Position]bool) {
	if !board.Contains(pos) {
		external[pos] = true
		return
	}
	if visited[pos] {
		if board.Get(pos) == r {
			return
		}
		external[pos] = true
		return
	}
	if r != board.Get(pos) {
		external[pos] = true
		return
	}
	visited[pos] = true
	internal[pos] = true
	sides(r, board, pos.Move(aoc.Up, 1), visited, internal, external)
	sides(r, board, pos.Move(aoc.Left, 1), visited, internal, external)
	sides(r, board, pos.Move(aoc.Right, 1), visited, internal, external)
	sides(r, board, pos.Move(aoc.Down, 1), visited, internal, external)
}

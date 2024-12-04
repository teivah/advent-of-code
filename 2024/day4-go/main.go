package main

import (
	"io"

	aoc "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	board := aoc.NewBoardFromReader(input, func(r rune) rune { return r })
	res := 0
	word := []rune("XMAS")
	for pos := range board.Positions {
		res += expand(board, pos, aoc.Up, word)
		res += expand(board, pos, aoc.Down, word)
		res += expand(board, pos, aoc.Left, word)
		res += expand(board, pos, aoc.Right, word)
		res += expand(board, pos, aoc.UpLeft, word)
		res += expand(board, pos, aoc.UpRight, word)
		res += expand(board, pos, aoc.DownLeft, word)
		res += expand(board, pos, aoc.DownRight, word)
	}
	return res
}

func expand(board aoc.Board[rune], pos aoc.Position, dir aoc.Direction, word []rune) int {
	if len(word) == 0 {
		return 1
	}
	if !board.Contains(pos) {
		return 0
	}
	if board.Get(pos) != word[0] {
		return 0
	}
	return expand(board, pos.Move(dir, 1), dir, word[1:])
}

func fs2(input io.Reader) int {
	board := aoc.NewBoardFromReader(input, func(r rune) rune { return r })
	res := 0
	for pos := range board.Positions {
		res += xmas(board, pos)
	}
	return res
}

func xmas(board aoc.Board[rune], pos aoc.Position) int {
	if board.Get(pos) != 'A' {
		return 0
	}
	upLeft := board.Get(pos.Move(aoc.UpLeft, 1))
	upRight := board.Get(pos.Move(aoc.UpRight, 1))
	downLeft := board.Get(pos.Move(aoc.DownLeft, 1))
	downRight := board.Get(pos.Move(aoc.DownRight, 1))

	// First diagonal
	switch upLeft {
	case 'M':
		if downRight != 'S' {
			return 0
		}
	case 'S':
		if downRight != 'M' {
			return 0
		}
	default:
		return 0
	}

	// First diagonal
	switch upRight {
	case 'M':
		if downLeft != 'S' {
			return 0
		}
	case 'S':
		if downLeft != 'M' {
			return 0
		}
	default:
		return 0
	}
	return 1
}

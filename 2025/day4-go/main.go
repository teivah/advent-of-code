package main

import (
	"io"

	"github.com/teivah/go-aoc"
)

func fs1(input io.Reader) int {
	board := aoc.NewBoardFromReader(input, func(row, col int, r rune) rune {
		return r
	})
	res := 0
	for row := 0; row < board.MaxCols; row++ {
		for col := 0; col < board.MaxCols; col++ {
			if board.Get(aoc.NewPosition(row, col)) == paper && count(board, row, col) < 4 {
				res++
			}
		}
	}
	return res
}

const paper rune = '@'

func count(board aoc.Board[rune], row, col int) int {
	res := 0
	if isPaper(board, row-1, col-1) {
		res++
	}
	if isPaper(board, row-1, col) {
		res++
	}
	if isPaper(board, row-1, col+1) {
		res++
	}
	if isPaper(board, row, col+1) {
		res++
	}
	if isPaper(board, row+1, col+1) {
		res++
	}
	if isPaper(board, row+1, col) {
		res++
	}
	if isPaper(board, row+1, col-1) {
		res++
	}
	if isPaper(board, row, col-1) {
		res++
	}
	return res
}

func isPaper(board aoc.Board[rune], row, col int) bool {
	p := aoc.NewPosition(row, col)
	return board.Contains(p) && board.Get(p) == paper
}

func fs2(input io.Reader) int {
	board := aoc.NewBoardFromReader(input, func(row, col int, r rune) rune {
		return r
	})
	res := 0
	for {
		add := 0
		var positions []aoc.Position
		for row := 0; row < board.MaxCols; row++ {
			for col := 0; col < board.MaxCols; col++ {
				p := aoc.NewPosition(row, col)
				if board.Get(p) == paper && count(board, row, col) < 4 {
					add++
					positions = append(positions, p)
				}
			}
		}
		if add == 0 {
			break
		}
		res += add
		for _, p := range positions {
			board.Positions[p] = '.'
		}
	}
	return res
}

package main

import (
	"io"

	"github.com/teivah/go-aoc"
)

type cell uint8

const (
	empty cell = iota
	filled
)

type inputType uint8

const (
	lock inputType = iota
	key
)

func fs1(input io.Reader) int {
	groups := aoc.StringGroups(aoc.ReaderToStrings(input))
	var locks [][]int
	var keys [][]int
	space := 0
	for _, group := range groups {
		board := aoc.ParseBoard(group, func(r rune, pos aoc.Position) cell {
			switch r {
			default:
				panic(r)
			case '.':
				return empty
			case '#':
				return filled
			}
		})
		space = board.MaxRows - 2
		t, pins := parse(board)
		if t == lock {
			locks = append(locks, pins)
		} else {
			keys = append(keys, pins)
		}
	}

	res := 0
	for _, k := range keys {
		for _, l := range locks {
			fit := true
			for i := 0; i < len(k); i++ {
				if k[i]+l[i] > space {
					fit = false
					break
				}
			}
			if fit {
				res++
			}
		}
	}
	return res
}

func parse(board aoc.Board[cell]) (inputType, []int) {
	t := key
	if isLock(board) {
		t = lock
	}

	var res []int
	if t == lock {
		for col := 0; col < board.MaxCols; col++ {
			row := 1
			for ; row < board.MaxRows; row++ {
				if board.Get(aoc.NewPosition(row, col)) != filled {
					break
				}
			}
			res = append(res, row-1)
		}
	} else {
		for col := 0; col < board.MaxCols; col++ {
			row := board.MaxRows - 2
			for ; row >= 0; row-- {
				if board.Get(aoc.NewPosition(row, col)) != filled {
					break
				}
			}
			res = append(res, board.MaxRows-row-2)
		}
	}
	return t, res
}

func isLock(board aoc.Board[cell]) bool {
	for col := 0; col < board.MaxCols; col++ {
		if board.Get(aoc.NewPosition(0, col)) != filled {
			return false
		}
	}
	return true
}

func fs2(input io.Reader) int {
	_ = aoc.ReaderToStrings(input)
	return 42
}

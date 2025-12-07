package main

import (
	"io"

	"github.com/teivah/go-aoc"
)

type cell = rune

const (
	empty    cell = '.'
	start    cell = 'S'
	splitter cell = '^'
	beam     cell = '|'
)

func fs1(input io.Reader) int {
	var pos aoc.Position
	board := aoc.ParseBoard(aoc.ReaderToStrings(input), func(r rune, p aoc.Position) cell {
		switch r {
		default:
			panic(string(r))
		case empty:
			return empty
		case start:
			pos = p.Move(aoc.Down, 1)
			return start
		case splitter:
			return splitter
		}
	})

	return count(pos, board)
}

func count(pos aoc.Position, board aoc.Board[cell]) int {
	if !board.Contains(pos) {
		return 0
	}
	c := board.Get(pos)
	switch c {
	default:
		panic(string(c))
	case empty:
		board.Positions[pos] = beam
		return 0 + count(pos.Move(aoc.Down, 1), board)
	case beam:
		return 0
	case splitter:
		return 1 +
			count(pos.Move(aoc.DownLeft, 1), board) +
			count(pos.Move(aoc.DownRight, 1), board)
	}
}

var cache map[aoc.Position]int

func fs2(input io.Reader) int {
	cache = make(map[aoc.Position]int)
	var pos aoc.Position
	board := aoc.ParseBoard(aoc.ReaderToStrings(input), func(r rune, p aoc.Position) cell {
		switch r {
		default:
			panic(string(r))
		case empty:
			return empty
		case start:
			pos = p.Move(aoc.Down, 1)
			return start
		case splitter:
			return splitter
		}
	})

	return count2(pos, board)
}

func count2(pos aoc.Position, board aoc.Board[cell]) int {
	if !board.Contains(pos) {
		return 1
	}

	if v, contains := cache[pos]; contains {
		return v
	}

	c := board.Get(pos)
	switch c {
	default:
		panic(string(c))
	case empty:
		return count2(pos.Move(aoc.Down, 1), board)
	case splitter:
		v := count2(pos.Move(aoc.DownLeft, 1), board) +
			count2(pos.Move(aoc.DownRight, 1), board)
		cache[pos] = v
		return v
	}
}

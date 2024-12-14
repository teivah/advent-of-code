package main

import (
	"io"

	"github.com/teivah/go-aoc"
)

func fs1(input io.Reader) int {
	board := aoc.ParseBoard(aoc.ReaderToStrings(input), func(r rune, _ aoc.Position) int {
		return aoc.RuneToInt(r)
	})

	count := 0
	for pos := range board.Positions {
		res := make(map[aoc.Position]struct{})
		solve1(board, pos, aoc.UnknownDirection, 0, res)
		count += len(res)
	}
	return count
}

func solve1(board aoc.Board[int], pos aoc.Position, dir aoc.Direction, expected int, res map[aoc.Position]struct{}) {
	if !board.Contains(pos) {
		return
	}
	cur := board.Get(pos)
	if cur != expected {
		return
	}
	if expected == 9 {
		res[pos] = struct{}{}
		return
	}

	rev := dir.Rev()
	up := pos.Move(aoc.Up, 1)
	down := pos.Move(aoc.Down, 1)
	left := pos.Move(aoc.Left, 1)
	right := pos.Move(aoc.Right, 1)

	if rev != aoc.Up {
		solve1(board, up, aoc.Up, expected+1, res)
	}
	if rev != aoc.Down {
		solve1(board, down, aoc.Down, expected+1, res)
	}
	if rev != aoc.Left {
		solve1(board, left, aoc.Left, expected+1, res)
	}
	if rev != aoc.Right {
		solve1(board, right, aoc.Right, expected+1, res)
	}
}

func fs2(input io.Reader) int {
	board := aoc.ParseBoard(aoc.ReaderToStrings(input), func(r rune, _ aoc.Position) int {
		return aoc.RuneToInt(r)
	})

	count := 0
	for pos := range board.Positions {
		count += solve2(board, pos, aoc.UnknownDirection, 0)
	}
	return count
}

func solve2(board aoc.Board[int], pos aoc.Position, dir aoc.Direction, expected int) int {
	if !board.Contains(pos) {
		return 0
	}
	cur := board.Get(pos)
	if cur != expected {
		return 0
	}
	if expected == 9 {
		return 1
	}

	rev := dir.Rev()
	up := pos.Move(aoc.Up, 1)
	down := pos.Move(aoc.Down, 1)
	left := pos.Move(aoc.Left, 1)
	right := pos.Move(aoc.Right, 1)

	count := 0
	if rev != aoc.Up {
		count += solve2(board, up, aoc.Up, expected+1)
	}
	if rev != aoc.Down {
		count += solve2(board, down, aoc.Down, expected+1)
	}
	if rev != aoc.Left {
		count += solve2(board, left, aoc.Left, expected+1)
	}
	if rev != aoc.Right {
		count += solve2(board, right, aoc.Right, expected+1)
	}
	return count
}

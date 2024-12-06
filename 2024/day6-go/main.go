package main

import (
	"io"

	"github.com/teivah/go-aoc"
)

type Cell struct {
	empty       bool
	obstruction bool
}

func fs1(input io.Reader) int {
	var pos aoc.Position
	dir := aoc.Up
	board := aoc.NewBoardFromReader(input, func(row, col int, r rune) Cell {
		switch r {
		default:
			panic(r)
		case '.':
			return Cell{empty: true}
		case '#':
			return Cell{obstruction: true}
		case '^':
			pos = aoc.NewPosition(row, col)
			return Cell{empty: true}
		}
	})

	visited := make(map[aoc.Position]struct{})
	loc := aoc.NewLocation(pos.Row, pos.Col, dir)
	for {
		visited[loc.Pos] = struct{}{}
		var stop bool
		loc, stop = move(board, loc)
		if stop {
			break
		}
	}

	return len(visited)
}

func move(board aoc.Board[Cell], loc aoc.Location) (aoc.Location, bool) {
	next := loc.Move(loc.Dir, 1)
	if !board.Contains(next.Pos) {
		return aoc.Location{}, true
	}

	switch {
	case board.Get(next.Pos).obstruction:
		return move(board, loc.Turn(aoc.Right, 0))
	case board.Get(next.Pos).empty:
		return next, false
	default:
		panic(board.Get(next.Pos))
	}
}

func fs2(input io.Reader) int {
	var pos aoc.Position
	dir := aoc.Up
	board := aoc.NewBoardFromReader(input, func(row, col int, r rune) Cell {
		switch r {
		default:
			panic(r)
		case '.':
			return Cell{empty: true}
		case '#':
			return Cell{obstruction: true}
		case '^':
			pos = aoc.NewPosition(row, col)
			return Cell{empty: true}
		}
	})

	res := 0
	for cur, cell := range board.Positions {
		switch {
		case cell.obstruction:
			continue
		case cell.empty:
			board.Positions[cur] = Cell{obstruction: true}
		default:
			panic(cell)
		}

		visited := make(map[aoc.Location]bool)
		loc := aoc.NewLocation(pos.Row, pos.Col, dir)
		loop := false
		for {
			if visited[loc] {
				loop = true
				break
			}

			visited[loc] = true
			var stop bool
			loc, stop = move(board, loc)
			if stop {
				break
			}
		}

		if loop {
			res++
		}
		board.Positions[cur] = Cell{empty: true}
	}

	return res
}

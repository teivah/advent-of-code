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

		local := 0

		horizontal := aoc.MapCopy(external)
		for pos := range horizontal {
			exists := make(map[aoc.Direction]bool)
			expand(pos, aoc.Left, internal, horizontal, exists)
			horizontal[pos] = true
			expand(pos, aoc.Right, internal, horizontal, exists)
			local += len(exists)
			//fmt.Println("horizontal", pos, len(exists))
		}

		vertical := aoc.MapCopy(external)
		for pos := range vertical {
			exists := make(map[aoc.Direction]bool)
			expand(pos, aoc.Up, internal, vertical, exists)
			vertical[pos] = true
			expand(pos, aoc.Down, internal, vertical, exists)
			local += len(exists)
			//fmt.Println("vertical", pos, len(exists))
		}
		//fmt.Println(after-before, local)
		res += (after - before) * local
	}
	return res
}

func expand(pos aoc.Position, dir aoc.Direction, internal, external map[aoc.Position]bool, exists map[aoc.Direction]bool) {
	if !external[pos] {
		return
	}

	switch dir {
	case aoc.Left, aoc.Right:
		if internal[pos.Move(aoc.Up, 1)] {
			exists[aoc.Up] = true
		}
		if internal[pos.Move(aoc.Down, 1)] {
			exists[aoc.Down] = true
		}
	case aoc.Up, aoc.Down:
		if internal[pos.Move(aoc.Left, 1)] {
			exists[aoc.Left] = true
		}
		if internal[pos.Move(aoc.Right, 1)] {
			exists[aoc.Right] = true
		}
	}
	delete(external, pos)
	expand(pos.Move(dir, 1), dir, internal, external, exists)
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

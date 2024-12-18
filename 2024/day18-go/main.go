package main

import (
	"fmt"
	"io"

	"github.com/teivah/go-aoc"
)

type cell uint32

const (
	safe cell = iota
	corrupted
)

func fs1(input io.Reader, memoryCount, rows, cols int) int {
	board := aoc.NewBoardFromLength(0, rows, 0, cols, safe)
	lines := aoc.ReaderToStrings(input)
	for i := 0; i < memoryCount; i++ {
		pos := parse(lines[i])
		board.Positions[pos] = corrupted
	}
	return bfs(board, aoc.NewPosition(0, 0), aoc.NewPosition(rows-1, cols-1))
}

func parse(s string) aoc.Position {
	del := aoc.NewDelimiter(s, ",")
	return aoc.NewPosition(del.GetInt(1), del.GetInt(0))
}

type state struct {
	steps int
	pos   aoc.Position
}

func bfs(board aoc.Board[cell], from, to aoc.Position) int {
	q := []state{{pos: from}}
	visited := make(map[aoc.Position]bool)
	for len(q) != 0 {
		s := q[0]
		q = q[1:]
		if s.pos == to {
			return s.steps
		}
		if !board.Contains(s.pos) {
			continue
		}
		if board.Get(s.pos) == corrupted {
			continue
		}
		if visited[s.pos] {
			continue
		}
		visited[s.pos] = true

		steps := s.steps + 1
		q = append(q, state{steps: steps, pos: s.pos.Move(aoc.Up, 1)})
		q = append(q, state{steps: steps, pos: s.pos.Move(aoc.Down, 1)})
		q = append(q, state{steps: steps, pos: s.pos.Move(aoc.Left, 1)})
		q = append(q, state{steps: steps, pos: s.pos.Move(aoc.Right, 1)})
	}
	return -1
}

func fs2(input io.Reader, rows, cols int) string {
	board := aoc.NewBoardFromLength(0, rows, 0, cols, safe)
	lines := aoc.ReaderToStrings(input)
	for i := 0; i < len(lines); i++ {
		pos := parse(lines[i])
		board.Positions[pos] = corrupted
		moves := bfs(board, aoc.NewPosition(0, 0), aoc.NewPosition(rows-1, cols-1))
		if moves == -1 {
			return fmt.Sprintf("%d,%d", pos.Col, pos.Row)
		}
	}
	return ""
}

package main

import (
	"io"
	"math"

	aoc "github.com/teivah/advent-of-code"
)

func fs(input io.Reader, minStraight, maxStraight int) int {
	lines := aoc.ReaderToStrings(input)
	board, target := parse(lines)
	return bfs(board, target, minStraight, maxStraight)
}

func parse(lines []string) (map[aoc.Position]int, aoc.Position) {
	board := make(map[aoc.Position]int)
	for row, line := range lines {
		for col, c := range line {
			board[aoc.Position{Row: row, Col: col}] = int(c - '0')
		}
	}
	return board, aoc.Position{
		Row: len(lines) - 1,
		Col: len(lines[0]) - 1,
	}
}

func bfs(board map[aoc.Position]int, target aoc.Position, minStraight, maxStraight int) int {
	type queueEntry struct {
		pos      aoc.Position
		dir      aoc.Direction
		heatLoss int
		straight int
	}
	type cacheEntry struct {
		pos      aoc.Position
		dir      aoc.Direction
		straight int
	}

	q := []queueEntry{
		{
			pos:      aoc.Position{Row: 0, Col: 1},
			straight: 1,
			dir:      aoc.Right,
		},
		{
			pos:      aoc.Position{Row: 1, Col: 0},
			straight: 1,
			dir:      aoc.Down,
		},
	}
	cache := make(map[cacheEntry]int)
	best := math.MaxInt

	for len(q) != 0 {
		e := q[0]
		q = q[1:]

		if _, exists := board[e.pos]; !exists {
			continue
		}

		heat := board[e.pos] + e.heatLoss
		if e.pos == target {
			best = min(best, heat)
			continue
		}

		ce := cacheEntry{pos: e.pos, dir: e.dir, straight: e.straight}
		if v, exists := cache[ce]; exists {
			if v <= heat {
				continue
			}
		}
		cache[ce] = heat

		if e.straight >= minStraight {
			left := e.dir.Turn(aoc.Left)
			q = append(q, queueEntry{
				pos:      e.pos.Move(left, 1),
				dir:      left,
				heatLoss: heat,
				straight: 1,
			})

			right := e.dir.Turn(aoc.Right)
			q = append(q, queueEntry{
				pos:      e.pos.Move(right, 1),
				dir:      right,
				heatLoss: heat,
				straight: 1,
			})
		}

		if e.straight < maxStraight {
			q = append(q, queueEntry{
				pos:      e.pos.Move(e.dir, 1),
				dir:      e.dir,
				heatLoss: heat,
				straight: e.straight + 1,
			})
		}
	}
	return best
}

package main

import (
	"io"
	"math"

	aoc "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	lines := aoc.ReaderToStrings(input)
	board, target := parse(lines)
	return bfs1(board, target)
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

func bfs1(board map[aoc.Position]int, target aoc.Position) int {
	const maxStraight = 3

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
			straight: maxStraight - 1,
			dir:      aoc.Right,
		},
		{
			pos:      aoc.Position{Row: 1, Col: 0},
			straight: maxStraight - 1,
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

		left := e.dir.Turn(aoc.Left)
		q = append(q, queueEntry{
			pos:      e.pos.Move(left, 1),
			dir:      left,
			heatLoss: heat,
			straight: maxStraight - 1,
		})

		right := e.dir.Turn(aoc.Right)
		q = append(q, queueEntry{
			pos:      e.pos.Move(right, 1),
			dir:      right,
			heatLoss: heat,
			straight: maxStraight - 1,
		})

		// Straight
		if e.straight > 0 {
			q = append(q, queueEntry{
				pos:      e.pos.Move(e.dir, 1),
				dir:      e.dir,
				heatLoss: heat,
				straight: e.straight - 1,
			})
		}
	}
	return best
}

func fs2(input io.Reader) int {
	lines := aoc.ReaderToStrings(input)
	board, target := parse(lines)
	return bfs2(board, target)
}

func bfs2(board map[aoc.Position]int, target aoc.Position) int {
	const (
		minStraight = 4
		maxStraight = 10
	)

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

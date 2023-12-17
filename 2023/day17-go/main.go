package main

import (
	"bufio"
	"io"
	"math"

	aoc "github.com/teivah/advent-of-code"
)

const singleDirectionMax = 3

func fs1(input io.Reader) int {
	lines := aoc.ReaderToStrings(input)
	board, target := parse(lines)
	return bfs(board, target)
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

func bfs(board map[aoc.Position]int, target aoc.Position) int {
	type queueEntry struct {
		pos                      aoc.Position
		dir                      aoc.Direction
		heatLoss                 int
		singleDirectionRemaining int
	}
	type cacheEntry struct {
		pos                      aoc.Position
		dir                      aoc.Direction
		singleDirectionRemaining int
	}

	q := []queueEntry{
		{
			pos:                      aoc.Position{Row: 0, Col: 1},
			singleDirectionRemaining: singleDirectionMax - 1,
			dir:                      aoc.Right,
		},
		{
			pos:                      aoc.Position{Row: 1, Col: 0},
			singleDirectionRemaining: singleDirectionMax - 1,
			dir:                      aoc.Down,
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

		ce := cacheEntry{pos: e.pos, dir: e.dir, singleDirectionRemaining: e.singleDirectionRemaining}
		if v, exists := cache[ce]; exists {
			if v <= heat {
				continue
			}
		}
		cache[ce] = heat

		// Left
		left := e.dir.Turn(aoc.Left)
		q = append(q, queueEntry{
			pos:                      e.pos.Move(left, 1),
			dir:                      left,
			heatLoss:                 heat,
			singleDirectionRemaining: singleDirectionMax - 1,
		})
		// Right
		right := e.dir.Turn(aoc.Right)
		q = append(q, queueEntry{
			pos:                      e.pos.Move(right, 1),
			dir:                      right,
			heatLoss:                 heat,
			singleDirectionRemaining: singleDirectionMax - 1,
		})
		// Straight
		if e.singleDirectionRemaining > 0 {
			q = append(q, queueEntry{
				pos:                      e.pos.Move(e.dir, 1),
				dir:                      e.dir,
				heatLoss:                 heat,
				singleDirectionRemaining: e.singleDirectionRemaining - 1,
			})
		}
	}
	return best
}

func fs2(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
		line := scanner.Text()
		_ = line
	}

	return 42
}

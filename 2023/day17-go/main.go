package main

import (
	"bufio"
	"io"
	"math"

	aoc "github.com/teivah/advent-of-code"
)

const singleDirection = 3

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
		dir aoc.Direction
		heatLoss                 int
		singleDirectionRemaining int
	}
	type cacheEntry struct {
		pos aoc.Position
		dir aoc.Direction
	}

	q := []queueEntry{
		{
			pos:                      aoc.Position{Row: 0, Col: 1},
			singleDirectionRemaining: singleDirection - 1,
		},
		{
			pos:                      aoc.Position{Row: 1, Col: 0},
			singleDirectionRemaining: singleDirection - 1,
		},
	}
	cache := make(map[cacheEntry]int)
	best := math.MaxInt

	for len(q) != 0 {
		e := q[0]
		q = q[1:]

		if e.pos == target {
			best = min(best, e.heatLoss)
			continue
		}

		ce := cacheEntry{pos: e.pos, dir:e.dir}
		if v, exists := cache[ce]; exists {
			if v <= e.heatLoss {
				continue
			}
		}
		cache[ce] = e.heatLoss

		// Left
		e.dir.
	}
	panic("no result found")
}

func fs2(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
		line := scanner.Text()
		_ = line
	}

	return 42
}

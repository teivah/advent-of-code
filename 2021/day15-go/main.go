package main

import (
	"bufio"
	"io"

	pq "github.com/emirpasic/gods/queues/priorityqueue"
	aoc "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	lines := aoc.ReaderToStrings(input)
	grid := make(map[aoc.Position]int)
	for row, line := range lines {
		for col := 0; col < len(line); col++ {
			r := rune(line[col])
			grid[aoc.Position{row, col}] = aoc.RuneToInt(r)
		}
	}

	return bfs(grid, aoc.Position{
		Row: len(lines) - 1,
		Col: len(lines[0]) - 1,
	})
}

type Entry struct {
	pos  aoc.Position
	risk int
}

func bfs(grid map[aoc.Position]int, target aoc.Position) int {
	q := pq.NewWith(func(a, b interface{}) int {
		priorityA := a.(Entry).risk
		priorityB := b.(Entry).risk
		return priorityA - priorityB
	})
	q.Enqueue(Entry{})
	visited := make(map[aoc.Position]int)

	best := aoc.NewMiner()
	for !q.Empty() {
		x, _ := q.Dequeue()
		e := x.(Entry)

		if e.pos == target {
			best.Add(e.risk)
			continue
		}

		if risk, exists := visited[e.pos]; exists {
			if risk <= e.risk {
				continue
			}
		}
		visited[e.pos] = e.risk

		e.risk += grid[e.pos]

		p := e.pos.Delta(-1, 0)
		if _, exists := grid[p]; exists {
			q.Enqueue(Entry{
				pos:  p,
				risk: e.risk,
			})
		}

		p = e.pos.Delta(1, 0)
		if _, exists := grid[p]; exists {
			q.Enqueue(Entry{
				pos:  p,
				risk: e.risk,
			})
		}

		p = e.pos.Delta(0, -1)
		if _, exists := grid[p]; exists {
			q.Enqueue(Entry{
				pos:  p,
				risk: e.risk,
			})
		}

		p = e.pos.Delta(0, 1)
		if _, exists := grid[p]; exists {
			q.Enqueue(Entry{
				pos:  p,
				risk: e.risk,
			})
		}
	}

	return best.Get()
}

func fs2(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
		line := scanner.Text()
		_ = line
	}

	return 42
}

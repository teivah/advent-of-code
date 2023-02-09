package main

import (
	"io"

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

	return findBest(grid, aoc.Position{
		Row: len(lines) - 1,
		Col: len(lines[0]) - 1,
	})
}

type Entry struct {
	pos  aoc.Position
	risk int
}

func findBest(grid map[aoc.Position]int, target aoc.Position) int {
	q := []Entry{{}}
	visited := make(map[aoc.Position]int)

	best := aoc.NewMiner()
	for len(q) != 0 {
		e := q[0]
		q = q[1:]

		if e.pos == target {
			best.Add(e.risk + grid[target] - grid[aoc.Position{}])
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
			q = append(q, Entry{
				pos:  p,
				risk: e.risk,
			})
		}

		p = e.pos.Delta(1, 0)
		if _, exists := grid[p]; exists {
			q = append(q, Entry{
				pos:  p,
				risk: e.risk,
			})
		}

		p = e.pos.Delta(0, -1)
		if _, exists := grid[p]; exists {
			q = append(q, Entry{
				pos:  p,
				risk: e.risk,
			})
		}

		p = e.pos.Delta(0, 1)
		if _, exists := grid[p]; exists {
			q = append(q, Entry{
				pos:  p,
				risk: e.risk,
			})
		}
	}

	return best.Get()
}

func fs2(input io.Reader) int {
	lines := aoc.ReaderToStrings(input)
	grid := make(map[aoc.Position]int)
	for row, line := range lines {
		for col := 0; col < len(line); col++ {
			r := rune(line[col])
			grid[aoc.Position{row, col}] = aoc.RuneToInt(r)
		}
	}

	maxRow := aoc.NewMaxer()
	maxCol := aoc.NewMaxer()
	for deltaRow := 0; deltaRow < 5; deltaRow++ {
		for deltaCol := 0; deltaCol < 5; deltaCol++ {
			if deltaRow == 0 && deltaCol == 0 {
				continue
			}

			delta := deltaRow + deltaCol
			for row, line := range lines {
				for col := 0; col < len(line); col++ {
					r := deltaRow*len(lines) + row
					c := deltaCol*len(lines[0]) + col
					v := grid[aoc.Position{row, col}] + delta
					if v > 9 {
						v = (v % 10) + 1
					}
					grid[aoc.Position{r, c}] =
						v
					maxRow.Add(r)
					maxCol.Add(c)
				}
			}
		}
	}

	return findBest(grid, aoc.Position{
		Row: maxRow.Get(),
		Col: maxCol.Get(),
	})
}

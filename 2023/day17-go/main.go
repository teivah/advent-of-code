package main

import (
	"io"

	pq "github.com/emirpasic/gods/queues/priorityqueue"
	aoc "github.com/teivah/advent-of-code"
)

func fs(input io.Reader, minStraight, maxStraight int) int {
	lines := aoc.ReaderToStrings(input)
	board, target := parse(lines)
	return shortest(board, target, minStraight, maxStraight)
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

func shortest(board map[aoc.Position]int, target aoc.Position, minStraight, maxStraight int) int {
	type state struct {
		loc      aoc.Location
		straight int
	}
	type entry struct {
		state
		heatLoss int
	}

	q := pq.NewWith(func(a, b any) int {
		p1 := a.(entry).heatLoss
		p2 := b.(entry).heatLoss
		return p1 - p2
	})

	q.Enqueue(entry{
		state: state{
			loc: aoc.Location{
				Pos: aoc.Position{Row: 0, Col: 1},
				Dir: aoc.Right,
			},
			straight: 1,
		},
	})
	q.Enqueue(entry{
		state: state{
			loc: aoc.Location{
				Pos: aoc.Position{Row: 1, Col: 0},
				Dir: aoc.Down,
			},
			straight: 1,
		},
	})
	visited := make(map[state]int)

	for !q.Empty() {
		t, _ := q.Dequeue()
		e := t.(entry)
		pos := e.loc.Pos

		if _, exists := board[pos]; !exists {
			continue
		}

		heat := board[pos] + e.heatLoss
		if pos == target {
			// Thanks to the priority queue, at this stage we already know this is the
			// shortest path.
			return heat
		}

		if v, exists := visited[e.state]; exists {
			if v <= heat {
				continue
			}
		}
		visited[e.state] = heat

		if e.straight >= minStraight {
			q.Enqueue(entry{
				state: state{
					loc:      e.loc.Turn(aoc.Left, 1),
					straight: 1,
				},
				heatLoss: heat,
			})

			q.Enqueue(entry{
				state: state{
					loc:      e.loc.Turn(aoc.Right, 1),
					straight: 1,
				},
				heatLoss: heat,
			})
		}

		if e.straight < maxStraight {
			q.Enqueue(entry{
				state: state{
					loc:      e.loc.Straight(1),
					straight: e.straight + 1,
				},
				heatLoss: heat,
			})
		}
	}
	panic("no result found")
}

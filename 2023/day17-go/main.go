package main

import (
	"io"

	aoc "github.com/teivah/advent-of-code"
)

func fs(input io.Reader, minStraight, maxStraight int) int {
	board, rows, cols := aoc.ParseBoard[int](input, aoc.RuneToInt)
	return shortest(board, aoc.NewPosition(rows-1, cols-1), minStraight, maxStraight)
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

	q := aoc.NewPriorityQueue[entry](func(a, b entry) int {
		return a.heatLoss - b.heatLoss
	})

	q.Push(entry{
		state: state{
			loc:      aoc.NewLocation(0, 1, aoc.Right),
			straight: 1,
		},
	})
	q.Push(entry{
		state: state{
			loc:      aoc.NewLocation(1, 0, aoc.Down),
			straight: 1,
		},
	})
	visited := make(map[state]int)

	for !q.IsEmpty() {
		e, _ := q.Pop()
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
			q.Push(entry{
				state: state{
					loc:      e.loc.Turn(aoc.Left, 1),
					straight: 1,
				},
				heatLoss: heat,
			})

			q.Push(entry{
				state: state{
					loc:      e.loc.Turn(aoc.Right, 1),
					straight: 1,
				},
				heatLoss: heat,
			})
		}

		if e.straight < maxStraight {
			q.Push(entry{
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

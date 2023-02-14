package main

import (
	"fmt"
	"io"

	aoc "github.com/teivah/advent-of-code"
)

func fs(input io.Reader) int {
	lines := aoc.ReaderToStrings(input)
	sea := toSea(lines)

	previous := fmt.Sprintf("%v", sea)
	for i := 1; ; i++ {
		sea.move()
		next := fmt.Sprintf("%v", sea)
		if previous == next {
			return i
		}
		previous = next
	}
}

func toSea(lines []string) *Sea {
	grid := make(map[aoc.Position]Unit, 0)
	for row := 0; row < len(lines); row++ {
		for col := 0; col < len(lines[row]); col++ {
			var unit Unit
			switch lines[row][col] {
			case '.':
				unit = empty
			case '>':
				unit = right
			case 'v':
				unit = down
			}
			grid[aoc.Position{row, col}] = unit
		}
	}
	return &Sea{
		grid: grid,
		row:  len(lines),
		col:  len(lines[0]),
	}
}

type Sea struct {
	grid map[aoc.Position]Unit
	row  int
	col  int
}

func (s *Sea) String() string {
	res := ""
	for row := 0; row < s.row; row++ {
		for col := 0; col < s.col; col++ {
			res += string(s.grid[aoc.Position{row, col}])
		}
		res += "\n"
	}
	return res
}

func (s *Sea) move() {
	res := make(map[aoc.Position]Unit, len(s.grid))
	added := make(map[aoc.Position]bool)
	for pos, unit := range s.grid {
		if unit == right {
			pos2 := pos.Move(aoc.Right, 1)
			if pos2.Col == s.col {
				pos2.Col = 0
			}
			if s.grid[pos2] == empty {
				res[pos] = empty
				res[pos2] = right
				added[pos2] = true
			} else {
				res[pos] = right
			}
		} else {
			if !added[pos] {
				res[pos] = unit
			}
		}
	}
	s.grid = res

	res = make(map[aoc.Position]Unit, len(s.grid))
	added = make(map[aoc.Position]bool)
	for pos, unit := range s.grid {
		if unit == down {
			pos2 := pos.Move(aoc.Down, 1)
			if pos2.Row == s.row {
				pos2.Row = 0
			}
			if s.grid[pos2] == empty {
				res[pos] = empty
				res[pos2] = down
				added[pos2] = true
			} else {
				res[pos] = down
			}
		} else {
			if !added[pos] {
				res[pos] = unit
			}
		}
	}
	s.grid = res
}

type Unit string

const (
	empty Unit = "."
	right Unit = ">"
	down  Unit = "v"
)

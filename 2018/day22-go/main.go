package main

import (
	"fmt"

	lib "github.com/teivah/advent-of-code"
)

func fs1(depth, col, row int) int {
	cave := toCave(depth, col, row)
	fmt.Println(cave)
	return cave.riskLevel()
}

type Cave struct {
	board     [][]Unit
	col       int
	row       int
	targetCol int
	targetRow int
}

func (c *Cave) riskLevel() int {
	risk := 0
	for row := 0; row < c.targetRow+1; row++ {
		for col := 0; col < c.targetCol+1; col++ {
			switch c.board[row][col].unitType {
			case wet:
				risk++
			case narrow:
				risk += 2
			}
		}
	}
	return risk
}

func (c *Cave) String() string {
	s := ""
	for row := 0; row < c.row; row++ {
		for col := 0; col < c.col; col++ {
			if row == 0 && col == 0 {
				s += "M"
				continue
			}
			if row == c.targetRow && col == c.targetCol {
				s += "T"
				continue
			}
			switch c.board[row][col].unitType {
			case rocky:
				s += "."
			case wet:
				s += "="
			case narrow:
				s += "|"
			}
		}
		s += "\n"
	}
	return s
}

func toCave(depth, targetCol, targetRow int) *Cave {
	const (
		bufferRow = 100
		bufferCol = 100
	)

	origCol := targetCol
	origRow := targetRow
	targetCol += bufferCol
	targetRow += bufferRow

	board := make([][]Unit, targetRow)

	for row := 0; row < targetRow; row++ {
		board[row] = make([]Unit, 0, targetCol+1)
		for col := 0; col < targetCol+1; col++ {
			gi := 0
			if row == 0 && col == 0 {
				gi = 0
			} else if row == targetRow && col == targetCol {
				gi = 0
			} else if row == 0 {
				gi = col * 16807
			} else if col == 0 {
				gi = row * 48271
			} else {
				gi = board[row][col-1].erosionLevel * board[row-1][col].erosionLevel
			}

			el := (gi + depth) % 20183

			var ut UnitType
			switch el % 3 {
			case 0:
				ut = rocky
			case 1:
				ut = wet
			case 2:
				ut = narrow
			}

			unit := Unit{
				unitType:      ut,
				erosionLevel:  el,
				geologicIndex: gi,
			}
			board[row] = append(board[row], unit)
		}
	}

	return &Cave{
		board:     board,
		col:       targetCol,
		row:       targetRow,
		targetCol: origCol,
		targetRow: origRow,
	}
}

type Unit struct {
	unitType      UnitType
	erosionLevel  int
	geologicIndex int
}

type UnitType int

const (
	rocky UnitType = iota
	wet
	narrow
)

type ToolType int

const (
	torch ToolType = iota
	gear
	none
)

func (c *Cave) isAllowed(pos Position, tool ToolType) bool {
	ut := c.board[pos.row][pos.col].unitType
	switch ut {
	case rocky:
		return tool == gear || tool == torch
	case wet:
		return tool == gear || tool == none
	case narrow:
		return tool == torch || tool == none
	}
	panic(ut)
}

func (c *Cave) getTools(pos Position) []ToolType {
	ut := c.board[pos.row][pos.col].unitType
	switch ut {
	case rocky:
		return []ToolType{gear, torch}
	case wet:
		return []ToolType{gear, none}
	case narrow:
		return []ToolType{torch, none}
	}
	panic(ut)
}

func (c *Cave) bfs() int {
	type State struct {
		minute int
		pos    Position
		tool   ToolType
	}
	q := []State{{}}
	target := Position{c.targetRow, c.targetCol}
	miner := lib.NewMiner()

	visited := make(map[ToolType]map[Position]int)

	for len(q) != 0 {
		state := q[0]
		q = q[1:]

		if state.pos == target {
			if state.tool == torch {
				miner.Add(state.minute)
			} else {
				// Switching tool
				miner.Add(state.minute + 7)
			}
			continue
		}

		if m, exists := visited[state.tool]; exists {
			if minute, exists := m[state.pos]; exists {
				if minute <= state.minute {
					continue
				}
			}
		} else {
			visited[state.tool] = make(map[Position]int)
		}

		visited[state.tool][state.pos] = state.minute

		add := func(next Position) {
			if next.isValid(c) {
				tools := c.getTools(next)
				for _, tool := range tools {
					if c.isAllowed(state.pos, tool) {
						if tool == state.tool {
							q = append(q, State{minute: state.minute + 1, pos: next, tool: tool})
						} else {
							q = append(q, State{minute: state.minute + 7, pos: state.pos, tool: tool})
						}
					}
				}
			}
		}

		add(state.pos.delta(-1, 0))
		add(state.pos.delta(1, 0))
		add(state.pos.delta(0, -1))
		add(state.pos.delta(0, 1))
	}

	return miner.Get()
}

type Position struct {
	row int
	col int
}

func (p Position) isValid(c *Cave) bool {
	if p.row < 0 || p.col < 0 || p.row >= c.row || p.col >= c.col {
		return false
	}
	return true
}

func (p Position) delta(row, col int) Position {
	p.row += row
	p.col += col
	return p
}

func fs2(depth, col, row int) int {
	cave := toCave(depth, col, row)
	return cave.bfs()
}

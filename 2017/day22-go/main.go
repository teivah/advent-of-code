package main

import (
	"bufio"
	"io"
)

func fs1(input io.Reader, rounds int) int {
	scanner := bufio.NewScanner(input)
	var grid [][]bool
	for scanner.Scan() {
		line := scanner.Text()
		var row []bool
		for i := 0; i < len(line); i++ {
			r := rune(line[i])
			switch r {
			case '.':
				row = append(row, false)
			case '#':
				row = append(row, true)
			default:
				panic(line)
			}
		}
		grid = append(grid, row)
	}

	g := &Grid{infected: grid, row: len(grid) / 2, col: len(grid[0]) / 2, heading: Up}

	for i := 0; i < rounds; i++ {
		g.step()
	}

	return g.infections
}

type Grid struct {
	// Step 1
	infected [][]bool
	// Step 2
	nodes      [][]Node
	row        int
	col        int
	heading    Heading
	infections int
}

type Heading int

const (
	Up Heading = iota
	Down
	Left
	Right
)

func (g *Grid) step() {
	// Step 1
	if g.infected[g.row][g.col] {
		g.turnRight()
	} else {
		g.turnLeft()
	}

	// Step 2
	g.infected[g.row][g.col] = !g.infected[g.row][g.col]
	if g.infected[g.row][g.col] {
		g.infections++
	}

	// Step 3
	g.move()
}

func (g *Grid) turnRight() {
	switch g.heading {
	case Up:
		g.heading = Right
	case Down:
		g.heading = Left
	case Left:
		g.heading = Up
	case Right:
		g.heading = Down
	}
}

func (g *Grid) turnLeft() {
	switch g.heading {
	case Up:
		g.heading = Left
	case Down:
		g.heading = Right
	case Left:
		g.heading = Down
	case Right:
		g.heading = Up
	}
}

func (g *Grid) move() {
	row := g.row
	col := g.col

	switch g.heading {
	case Up:
		row--
	case Down:
		row++
	case Left:
		col--
	case Right:
		col++
	}

	if row == -1 {
		res := make([][]bool, 0, len(g.infected)+1)
		res = append(res, make([]bool, len(g.infected[0])))
		for _, row := range g.infected {
			res = append(res, row)
		}
		g.infected = res
	} else if col == -1 {
		res := make([][]bool, 0, len(g.infected))
		for _, r := range g.infected {
			row := make([]bool, 0, len(g.infected[0])+1)
			row = append(row, false)
			for _, v := range r {
				row = append(row, v)
			}
			res = append(res, row)
		}
		g.infected = res
	} else if row == len(g.infected) {
		res := make([][]bool, 0, len(g.infected)+1)
		for _, row := range g.infected {
			res = append(res, row)
		}
		res = append(res, make([]bool, len(g.infected[0])))
		g.infected = res
		g.row++
	} else if col == len(g.infected[0]) {
		res := make([][]bool, 0, len(g.infected))
		for _, r := range g.infected {
			row := make([]bool, 0, len(g.infected[0])+1)
			for _, v := range r {
				row = append(row, v)
			}
			row = append(row, false)
			res = append(res, row)
		}
		g.infected = res
		g.col++
	} else {
		g.row = row
		g.col = col
	}
}

func (g *Grid) String() string {
	s := ""

	if g.infected != nil {
		for r, row := range g.infected {
			for c, v := range row {
				if r == g.row && c == g.col {
					s += "["
				} else {
					s += " "
				}
				if v {
					s += "#"
				} else {
					s += "."
				}
				if r == g.row && c == g.col {
					s += "]"
				} else {
					s += " "
				}
			}
			s += "\n"
		}
		return s
	}

	for r, row := range g.nodes {
		for c, v := range row {
			if r == g.row && c == g.col {
				s += "["
			} else {
				s += " "
			}

			switch v.nodeType {
			case Clean:
				s += "."
			case Weakened:
				s += "W"
			case Infected:
				s += "#"
			case Flagged:
				s += "F"
			}

			if r == g.row && c == g.col {
				s += "]"
			} else {
				s += " "
			}
		}
		s += "\n"
	}
	return s
}

func fs2(input io.Reader, rounds int) int {
	scanner := bufio.NewScanner(input)
	var nodes [][]Node
	for scanner.Scan() {
		line := scanner.Text()
		var row []Node
		for i := 0; i < len(line); i++ {
			r := rune(line[i])
			switch r {
			case '.':
				row = append(row, Node{Clean})
			case '#':
				row = append(row, Node{Infected})
			default:
				panic(line)
			}
		}
		nodes = append(nodes, row)
	}

	g := &Grid{nodes: nodes, row: len(nodes) / 2, col: len(nodes[0]) / 2, heading: Up}

	for i := 0; i < rounds; i++ {
		g.step2()
	}

	return g.infections
}

func (g *Grid) step2() {
	// Step 1
	node := g.nodes[g.row][g.col]
	switch node.nodeType {
	case Clean:
		g.turnLeft()
	case Weakened:
	case Infected:
		g.turnRight()
	case Flagged:
		// Revert direction
		g.turnLeft()
		g.turnLeft()
	}

	// Step 2
	switch node.nodeType {
	case Clean:
		g.nodes[g.row][g.col].nodeType = Weakened
	case Weakened:
		g.nodes[g.row][g.col].nodeType = Infected
		g.infections++
	case Infected:
		g.nodes[g.row][g.col].nodeType = Flagged
	case Flagged:
		g.nodes[g.row][g.col].nodeType = Clean
	}

	// Step 3
	g.move2()
}

func (g *Grid) move2() {
	row := g.row
	col := g.col

	switch g.heading {
	case Up:
		row--
	case Down:
		row++
	case Left:
		col--
	case Right:
		col++
	}

	if row == -1 {
		res := make([][]Node, 0, len(g.nodes)+1)
		res = append(res, make([]Node, len(g.nodes[0])))
		for _, row := range g.nodes {
			res = append(res, row)
		}
		g.nodes = res
	} else if col == -1 {
		res := make([][]Node, 0, len(g.nodes))
		for _, r := range g.nodes {
			row := make([]Node, 0, len(g.nodes[0])+1)
			row = append(row, Node{Clean})
			for _, v := range r {
				row = append(row, v)
			}
			res = append(res, row)
		}
		g.nodes = res
	} else if row == len(g.nodes) {
		res := make([][]Node, 0, len(g.nodes)+1)
		for _, row := range g.nodes {
			res = append(res, row)
		}
		res = append(res, make([]Node, len(g.nodes[0])))
		g.nodes = res
		g.row++
	} else if col == len(g.nodes[0]) {
		res := make([][]Node, 0, len(g.nodes))
		for _, r := range g.nodes {
			row := make([]Node, 0, len(g.nodes[0])+1)
			for _, v := range r {
				row = append(row, v)
			}
			row = append(row, Node{Clean})
			res = append(res, row)
		}
		g.nodes = res
		g.col++
	} else {
		g.row = row
		g.col = col
	}
}

type Node struct {
	nodeType NodeType
}

type NodeType int

const (
	Clean NodeType = iota
	Weakened
	Infected
	Flagged
)

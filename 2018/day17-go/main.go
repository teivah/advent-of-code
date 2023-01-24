package main

import (
	"bufio"
	"fmt"
	"io"
	"math"
	"strings"

	lib "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	var scans []Scan
	for scanner.Scan() {
		scans = append(scans, toScan(scanner.Text()))
	}

	const minRow = 0
	minCol := math.MaxInt
	maxCol := math.MinInt
	maxRow := 0
	for _, scan := range scans {
		if scan.rangeCol == nil {
			minCol = lib.Min(minCol, scan.col)
			maxCol = lib.Max(maxCol, scan.col)
		} else {
			minCol = lib.Min(minCol, scan.rangeCol.from)
			maxCol = lib.Max(maxCol, scan.rangeCol.to)
		}

		if scan.rangeRow == nil {
			maxRow = lib.Max(maxRow, scan.row)
		} else {
			maxRow = lib.Max(maxRow, scan.rangeRow.to)
		}
	}

	grid := newGrid(minRow, minCol, maxRow, maxCol, scans)

	grid.dfs(Position{0, 500})

	fmt.Println(grid)
	//fmt.Println(grid.Visited())
	//fmt.Println(grid.visited[Position{11, 500}])
	return grid.sumWater() - 1 // spring
}

type Grid struct {
	minRow int
	minCol int
	maxRow int
	maxCol int
	board  [][]UnitType
	spring Position
	end    map[Position]bool
}

type Position struct {
	row int
	col int
}

func (p Position) delta(row, col int) Position {
	p.row += row
	p.col += col
	return p
}

func (g *Grid) sumWater() int {
	sum := 0
	for _, row := range g.board {
		for _, v := range row {
			if v == water {
				sum++
			}
		}
	}
	return sum
}

func (g *Grid) unitType(current Position) *UnitType {
	if current.row < g.minRow || current.row > g.maxRow || current.col < g.minCol || current.col > g.maxCol {
		return nil
	}

	return &g.board[g.row(current.row)][g.col(current.col)]
}

func (g *Grid) dfs(current Position) bool {
	if current.row >= g.maxRow {
		g.board[g.row(current.row)][g.col(current.col)] = water
		g.end[current] = true
		return true
	}

	currentUnit := g.unitType(current)
	if currentUnit == nil {
		return false
	}
	if *currentUnit == clay || *currentUnit == water {
		return false
	}

	g.board[g.row(current.row)][g.col(current.col)] = water

	// Down?
	down := current.delta(1, 0)
	downUnit := g.unitType(down)
	if downUnit == nil {
		return false
	}

	if *downUnit == clay {
		left := g.dfs(current.delta(0, -1))
		right := g.dfs(current.delta(0, 1))
		res := left || right
		g.end[current] = res
		return res
	} else if *downUnit == water {
		if g.end[down] {
			return true
		}

		if g.ends(down) {
			g.end[down] = true
			return true
		}

		left := g.dfs(current.delta(0, -1))
		right := g.dfs(current.delta(0, 1))
		res := left || right
		g.end[current] = res
		return res
	} else { // sand
		if g.dfs(down) {
			return true
		}
		left := g.dfs(current.delta(0, -1))
		right := g.dfs(current.delta(0, 1))
		res := left || right
		g.end[current] = res
		return res
	}
}

func (g *Grid) ends(position Position) bool {
	q := []Position{position}
	visited := make(map[Position]struct{})

	for len(q) != 0 {
		pos := q[0]
		q = q[1:]

		if _, exists := visited[pos]; exists {
			continue
		}

		visited[pos] = struct{}{}
		unit := g.unitType(pos)
		if unit == nil || *unit != water {
			continue
		}

		if g.end[pos] {
			return true
		}

		q = append(q, pos.delta(1, 0))
		q = append(q, pos.delta(0, -1))
		q = append(q, pos.delta(0, 1))
	}

	return false
}

func (g *Grid) String() string {
	s := ""
	for _, row := range g.board {
		for _, t := range row {
			switch t {
			case sand:
				s += " "
			case clay:
				s += "#"
			case spring:
				s += "+"
			case water:
				s += "."
			}
		}
		s += "\n"
	}
	return s
}

//func (g *Grid) Visited() string {
//	s := ""
//	for r, row := range g.board {
//		for c := range row {
//			if g.visited[Position{r + g.minRow, c + g.minCol}] {
//				s += "X"
//			} else {
//				s += "."
//			}
//		}
//		s += "\n"
//	}
//	return s
//}

type UnitType int

const (
	sand UnitType = iota
	clay
	spring
	water
)

func newGrid(minRow int, minCol int, maxRow int, maxCol int, scans []Scan) *Grid {
	const buffer = 50
	minCol -= buffer
	maxCol += buffer

	board := make([][]UnitType, maxRow-minRow+1)
	for row := range board {
		board[row] = make([]UnitType, maxCol-minCol+1)
	}

	g := &Grid{minRow: minRow,
		minCol: minCol,
		maxRow: maxRow,
		maxCol: maxCol,
		board:  board,
		end:    make(map[Position]bool),
	}

	g.board[g.row(0)][g.col(500)] = spring
	g.spring = Position{g.row(0), g.col(500)}

	for _, scan := range scans {
		if scan.rangeCol != nil {
			row := scan.row
			for col := scan.rangeCol.from; col <= scan.rangeCol.to; col++ {
				g.board[g.row(row)][g.col(col)] = clay
			}
		} else {
			col := scan.col
			for row := scan.rangeRow.from; row <= scan.rangeRow.to; row++ {
				g.board[g.row(row)][g.col(col)] = clay
			}
		}
	}

	return g
}

func (g *Grid) col(i int) int {
	return i - g.minCol
}

func (g *Grid) row(i int) int {
	return i
}

type Scan struct {
	col      int
	rangeCol *Range
	row      int
	rangeRow *Range
}

func (s Scan) String() string {
	res := ""
	if s.rangeCol == nil {
		res += fmt.Sprintf("x=%d, ", s.col)
	} else {
		res += fmt.Sprintf("x=%d..%d, ", s.rangeCol.from, s.rangeCol.to)
	}

	if s.rangeRow == nil {
		res += fmt.Sprintf("y=%d", s.row)
	} else {
		res += fmt.Sprintf("y=%d..%d", s.rangeRow.from, s.rangeRow.to)
	}

	return res + "\n"
}

type Range struct {
	from int
	to   int
}

func toScan(s string) Scan {
	spaces := lib.NewDelimiter(s, " ")

	a := s[:spaces.Ind[0]-1]
	b := s[spaces.Ind[0]+1:]

	scan := &Scan{}
	updateScan(a, scan)
	updateScan(b, scan)

	return *scan
}

func updateScan(s string, scan *Scan) {
	del := strings.Index(s, "..")

	if s[0] == 'x' {
		if del == -1 {
			scan.col = lib.StringToInt(s[2:])
		} else {
			scan.rangeCol = &Range{
				from: lib.StringToInt(s[2:del]),
				to:   lib.StringToInt(s[del+2:]),
			}
		}
	} else {
		if del == -1 {
			scan.row = lib.StringToInt(s[2:])
		} else {
			scan.rangeRow = &Range{
				from: lib.StringToInt(s[2:del]),
				to:   lib.StringToInt(s[del+2:]),
			}
		}
	}
}

func fs2(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
		line := scanner.Text()
		_ = line
	}

	return 42
}

package main

import (
	"bufio"
	"fmt"
	"io"
)

func fs1(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	var board [][]UnitType
	for scanner.Scan() {
		line := scanner.Text()
		var row []UnitType
		for i := 0; i < len(line); i++ {
			r := line[i]
			switch r {
			case '.':
				row = append(row, open)
			case '|':
				row = append(row, tree)
			case '#':
				row = append(row, lumb)
			default:
				panic(r)
			}
		}
		board = append(board, row)
	}

	g := &Grid{
		board: board,
	}

	for round := 0; round < 10; round++ {
		g.round()
	}

	return g.score()
}

type Grid struct {
	board [][]UnitType
}

func (g *Grid) countAdjacents(cpy [][]UnitType, row, col int) (nopen, ntree, nlumb int) {
	var adjacents []UnitType
	if row-1 >= 0 {
		adjacents = append(adjacents, cpy[row-1][col])
	}

	if row-1 >= 0 && col-1 >= 0 {
		adjacents = append(adjacents, cpy[row-1][col-1])
	}

	if row-1 >= 0 && col+1 < len(g.board[0]) {
		adjacents = append(adjacents, cpy[row-1][col+1])
	}

	if col-1 >= 0 {
		adjacents = append(adjacents, cpy[row][col-1])
	}

	if col+1 < len(g.board[0]) {
		adjacents = append(adjacents, cpy[row][col+1])
	}

	if row+1 < len(g.board) {
		adjacents = append(adjacents, cpy[row+1][col])
	}

	if row+1 < len(g.board) && col-1 >= 0 {
		adjacents = append(adjacents, cpy[row+1][col-1])
	}

	if row+1 < len(g.board) && col+1 < len(g.board[0]) {
		adjacents = append(adjacents, cpy[row+1][col+1])
	}

	for _, adjacent := range adjacents {
		switch adjacent {
		case open:
			nopen++
		case tree:
			ntree++
		case lumb:
			nlumb++
		}
	}

	return
}

func (g *Grid) round() {
	cpy := make([][]UnitType, len(g.board))
	for row := 0; row < len(g.board); row++ {
		r := make([]UnitType, len(g.board[row]))
		copy(r, g.board[row])
		cpy[row] = r
	}

	for row := 0; row < len(g.board); row++ {
		for col := 0; col < len(g.board[row]); col++ {
			_, ntree, nlumb := g.countAdjacents(cpy, row, col)
			switch g.board[row][col] {
			case open:
				if ntree >= 3 {
					g.board[row][col] = tree
				}
			case tree:
				if nlumb >= 3 {
					g.board[row][col] = lumb
				}
			case lumb:
				if nlumb >= 1 && ntree >= 1 {
				} else {
					g.board[row][col] = open
				}
			}
		}
	}
}

func (g *Grid) String() string {
	s := ""
	for _, row := range g.board {
		for _, v := range row {
			switch v {
			case open:
				s += "."
			case tree:
				s += "|"
			case lumb:
				s += "#"
			}
		}
		s += "\n"
	}
	return s
}

type UnitType int

const (
	open UnitType = iota
	tree
	lumb
)

func fs2(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	var board [][]UnitType
	for scanner.Scan() {
		line := scanner.Text()
		var row []UnitType
		for i := 0; i < len(line); i++ {
			r := line[i]
			switch r {
			case '.':
				row = append(row, open)
			case '|':
				row = append(row, tree)
			case '#':
				row = append(row, lumb)
			default:
				panic(r)
			}
		}
		board = append(board, row)
	}

	g := &Grid{
		board: board,
	}

	for round := 0; round < 1000; round++ {
		g.round()
		fmt.Println(round+1, g.score())
	}

	/*
		The value repeats every 28 rounds
		832+28*35714256=1000000000
		So we need to return the value after round 832
	*/
	return 208080
}

func (g *Grid) score() int {
	ntree := 0
	nlumb := 0
	for _, row := range g.board {
		for _, v := range row {
			switch v {
			case open:
			case tree:
				ntree++
			case lumb:
				nlumb++
			}
		}
	}

	return ntree * nlumb
}

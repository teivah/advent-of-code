package main

import (
	"bufio"
	"io"
	"math"

	lib "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	grid := toGrid(lib.ReaderToStrings(input))

	set := make(map[int]struct{})
	for i := 0; ; i++ {
		grid.round()
		rating := grid.biodiversityRating()
		if _, exists := set[rating]; exists {
			return rating
		}
		set[rating] = struct{}{}
	}
}

type Grid struct {
	board [][]bool
}

func (g *Grid) String() string {
	s := ""
	for row := 0; row < 5; row++ {
		for col := 0; col < 5; col++ {
			if g.isBug(lib.Position{row, col}) {
				s += "#"
			} else {
				s += "."
			}
		}
		s += "\n"
	}
	return s
}

func (g *Grid) biodiversityRating() int {
	n := 0
	sum := 0
	for row := 0; row < 5; row++ {
		for col := 0; col < 5; col++ {
			if g.isBug(lib.Position{row, col}) {
				sum += int(math.Pow(2., float64(n)))
			}
			n++
		}
	}
	return sum
}

func (g *Grid) round() {
	res := make([][]bool, 5)
	for i := 0; i < 5; i++ {
		res[i] = make([]bool, 5)
	}

	for row := 0; row < 5; row++ {
		for col := 0; col < 5; col++ {
			res[row][col] = g.toBug(lib.Position{row, col})
		}
	}

	g.board = res
}

func (g *Grid) toBug(pos lib.Position) bool {
	adjacents := []lib.Position{
		pos.Delta(-1, 0),
		pos.Delta(1, 0),
		pos.Delta(0, -1),
		pos.Delta(0, 1),
	}
	sum := 0
	for _, p := range adjacents {
		if g.isBug(p) {
			sum++
		}
	}

	if g.isBug(pos) {
		return sum == 1
	} else {
		return sum == 1 || sum == 2
	}
}

func (g *Grid) isBug(pos lib.Position) bool {
	if pos.Row < 0 || pos.Col < 0 || pos.Row >= 5 || pos.Col >= 5 {
		return false
	}
	return g.get(pos)
}

func (g *Grid) get(pos lib.Position) bool {
	return g.board[pos.Row][pos.Col]
}

func toGrid(lines []string) *Grid {
	var board [][]bool
	for _, line := range lines {
		var row []bool
		for i := 0; i < len(line); i++ {
			if line[i] == '#' {
				row = append(row, true)
			} else {
				row = append(row, false)
			}
		}
		board = append(board, row)
	}
	return &Grid{
		board: board,
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

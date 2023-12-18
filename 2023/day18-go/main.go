package main

import (
	"bufio"
	"fmt"
	"io"
	"math"
	"strconv"

	aoc "github.com/teivah/advent-of-code"
)

type Terrain struct {
	isGround bool
}

func fs1(input io.Reader) int {
	board := toBoard(input)
	board.MaxRows++
	board.MaxCols++

	for row := board.MinRows; row < board.MaxRows; row++ {
		for col := board.MinCols; col < board.MaxCols; col++ {
			// Expand right
			trench := false
			for c := col + 1; c < board.MaxCols; c++ {
				pos := aoc.Position{Row: row, Col: c}
				if _, exists := board.Positions[pos]; exists {
					trench = true
					break
				}
			}

			if trench {
				// Expand Left
				trench = false
				for c := col - 1; c >= board.MinCols; c-- {
					pos := aoc.Position{Row: row, Col: c}
					if _, exists := board.Positions[pos]; exists {
						trench = true
						break
					}
				}
			}

			if trench {
				// Expand Up
				trench = false
				for r := row - 1; r >= board.MinRows; r-- {
					pos := aoc.Position{Row: r, Col: col}
					if _, exists := board.Positions[pos]; exists {
						trench = true
						break
					}
				}
			}

			if trench {
				// Expand Down
				trench = false
				for r := row + 1; r < board.MaxRows; r++ {
					pos := aoc.Position{Row: r, Col: col}
					if _, exists := board.Positions[pos]; exists {
						trench = true
						break
					}
				}
			}

			if trench {
				continue
			}
			fill(board, aoc.Position{Row: row, Col: col})
		}
	}

	countTerrain := 0
	for row := board.MinRows; row < board.MaxRows; row++ {
		for col := board.MinCols; col < board.MaxCols; col++ {
			if t, exists := board.Positions[aoc.Position{Row: row, Col: col}]; exists {
				if t.isGround {
					countTerrain++
				}
			}
		}
	}

	print(board)

	return (board.MaxRows-board.MinRows)*(board.MaxCols-board.MinCols) - countTerrain
}

func print(b aoc.Board[Terrain]) {
	for row := b.MinRows; row < b.MaxRows; row++ {
		for col := b.MinCols; col < b.MaxCols; col++ {
			if t, exists := b.Positions[aoc.Position{Row: row, Col: col}]; exists {
				if t.isGround {
					fmt.Print(".")
				} else {
					fmt.Print("#")
				}
			} else {
				fmt.Print("?")
			}
		}
		fmt.Println()
	}
}

func toBoard(input io.Reader) aoc.Board[Terrain] {
	positions := make(map[aoc.Position]Terrain)
	pos := aoc.Position{}

	scanner := bufio.NewScanner(input)
	idx := -1
	for scanner.Scan() {
		idx++
		line := scanner.Text()
		del := aoc.NewDelimiter(line, " ")
		direction := del.GetString(0)
		count := del.GetInt(1)
		s := del.GetString(2)
		color := s[1 : len(s)-1]
		_ = color

		var dir aoc.Direction
		switch direction {
		case "R":
			dir = aoc.Right
		case "L":
			dir = aoc.Left
		case "U":
			dir = aoc.Up
		case "D":
			dir = aoc.Down
		default:
			panic(direction)
		}

		for i := 0; i < count; i++ {
			pos = pos.Move(dir, 1)
			positions[pos] = Terrain{}
		}
	}

	return aoc.NewBoard(positions)
}

func fill(board aoc.Board[Terrain], pos aoc.Position) {
	q := []aoc.Position{pos}
	for len(q) != 0 {
		p := q[0]
		q = q[1:]
		if p.Row < board.MinRows || p.Row >= board.MaxRows || p.Col < board.MinCols || p.Col >= board.MaxCols {
			continue
		}
		if _, exists := board.Positions[p]; exists {
			continue
		}

		board.Positions[p] = Terrain{isGround: true}
		q = append(q, p.Move(aoc.Left, 1))
		q = append(q, p.Move(aoc.Right, 1))
		q = append(q, p.Move(aoc.Up, 1))
		q = append(q, p.Move(aoc.Down, 1))
	}
}

func fs2(input io.Reader) int {
	board, minRows, minCols := toBoard2(input)

	for row := minRows; row < board.MaxRows; row++ {
		for col := minCols; col < board.MaxCols; col++ {
			// Expand right
			trench := false
			for c := col + 1; c < board.MaxCols; c++ {
				pos := aoc.Position{Row: row, Col: c}
				if _, exists := board.Positions[pos]; exists {
					trench = true
					break
				}
			}

			if trench {
				// Expand Left
				trench = false
				for c := col - 1; c >= minCols; c-- {
					pos := aoc.Position{Row: row, Col: c}
					if _, exists := board.Positions[pos]; exists {
						trench = true
						break
					}
				}
			}

			if trench {
				// Expand Up
				trench = false
				for r := row - 1; r >= minRows; r-- {
					pos := aoc.Position{Row: r, Col: col}
					if _, exists := board.Positions[pos]; exists {
						trench = true
						break
					}
				}
			}

			if trench {
				// Expand Down
				trench = false
				for r := row + 1; r < board.MaxRows; r++ {
					pos := aoc.Position{Row: r, Col: col}
					if _, exists := board.Positions[pos]; exists {
						trench = true
						break
					}
				}
			}

			if trench {
				continue
			}
			fill(board, aoc.Position{Row: row, Col: col})
		}
	}

	countTerrain := 0
	for row := minRows; row < board.MaxRows; row++ {
		for col := minCols; col < board.MaxCols; col++ {
			if t, exists := board.Positions[aoc.Position{Row: row, Col: col}]; exists {
				if t.isGround {
					countTerrain++
				}
			}
		}
	}

	return (board.MaxRows-minRows)*(board.MaxCols-minCols) - countTerrain
}

func toBoard2(input io.Reader) (aoc.Board[Terrain], int, int) {
	board := aoc.Board[Terrain]{
		Positions: make(map[aoc.Position]Terrain),
	}
	pos := aoc.Position{}
	maxRows := 0
	maxCols := 0
	minRows := math.MaxInt
	minCols := math.MaxInt

	scanner := bufio.NewScanner(input)
	idx := -1
	for scanner.Scan() {
		idx++
		line := scanner.Text()
		del := aoc.NewDelimiter(line, " ")
		s := del.GetString(2)
		color := s[2 : len(s)-1]

		distance := color[:5]
		v, err := strconv.ParseInt(distance, 16, 64)
		if err != nil {
			panic(err)
		}

		var dir aoc.Direction
		switch color[5] {
		case '0':
			dir = aoc.Right
		case '1':
			dir = aoc.Down
		case '2':
			dir = aoc.Left
		case '3':
			dir = aoc.Up
		default:
			panic(color)
		}

		for i := 0; i < int(v); i++ {
			pos = pos.Move(dir, 1)
			board.Positions[pos] = Terrain{}
		}

		maxRows = max(maxRows, pos.Row)
		maxCols = max(maxCols, pos.Col)
		minRows = min(minRows, pos.Row)
		minCols = min(minCols, pos.Col)
		board.Positions[pos] = Terrain{}
	}

	board.MaxRows = maxRows + 1
	board.MaxCols = maxCols + 1

	fmt.Println("parse")
	return board, minRows, minCols
}

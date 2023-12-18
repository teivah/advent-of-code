package main

import (
	"bufio"
	"fmt"
	"io"

	aoc "github.com/teivah/advent-of-code"
)

type Terrain struct {
	color string
}

func fs1(input io.Reader) int {
	board, _ := toBoard(input)

	for row := 0; row < board.Rows; row++ {
		for col := 0; col < board.Cols; col++ {
			if _, exists := board.Positions[aoc.Position{row, col}]; exists {
				fmt.Print("#")
			} else {
				fmt.Print(".")
			}
		}
		fmt.Println()
	}

	//for {
	//	pos, contains := getNextInside(board)
	//	if !contains {
	//		break
	//	}
	//	fmt.Println(pos)
	//	fill(board, pos)
	//}
	// TODO Generic
	fill(board, aoc.Position{1, 1})

	return len(board.Positions)
}

func toBoard(input io.Reader) (aoc.Board[Terrain], aoc.Position) {
	board := aoc.Board[Terrain]{
		Positions: make(map[aoc.Position]Terrain),
	}
	pos := aoc.Position{}
	rows := 0
	cols := 0

	var start aoc.Position
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

		if idx == 1 {
			switch dir {
			case aoc.Up:
				start = pos.Move(aoc.UpLeft, 1)
			case aoc.Down:
				start = pos.Move(aoc.DownLeft, 1)
			default:
				panic(dir)
			}
		}

		for i := 0; i < count; i++ {
			pos = pos.Move(dir, 1)
			board.Positions[pos] = Terrain{}
		}

		rows = max(rows, pos.Row)
		cols = max(cols, pos.Col)
		board.Positions[pos] = Terrain{
			color: color,
		}
	}

	board.Rows = rows + 1
	board.Cols = cols + 1

	return board, start
}

func getNextInside(board aoc.Board[Terrain]) (aoc.Position, bool) {
	for row := 0; row < board.Rows; row++ {
		for col := 0; col < board.Cols; col++ {
			start := aoc.Position{Row: row, Col: col}
			if _, exists := board.Positions[start]; exists {
				continue
			}

			edges := 0
			for r, c := row+1, col+1; r < board.Rows && c < board.Cols; r, c = r+1, c+1 {
				pos := aoc.Position{Row: r, Col: c}
				if _, exists := board.Positions[pos]; exists {
					edges++
				}
			}

			if edges%2 == 1 {
				return start, true
			}
		}
	}

	return aoc.Position{}, false
}

func fill(board aoc.Board[Terrain], pos aoc.Position) {
	if _, exists := board.Positions[pos]; exists {
		return
	}
	board.Positions[pos] = Terrain{}
	fill(board, pos.Move(aoc.Left, 1))
	fill(board, pos.Move(aoc.Right, 1))
	fill(board, pos.Move(aoc.Up, 1))
	fill(board, pos.Move(aoc.Down, 1))
}

func fs2(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
		line := scanner.Text()
		_ = line
	}

	return 42
}

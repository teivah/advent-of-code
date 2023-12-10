package main

import (
	"fmt"
	"io"

	aoc "github.com/teivah/advent-of-code"
)

type tileType int

const (
	none tileType = iota
	vertical
	horizontal
	bendL
	bendJ
	bend7
	bendF
	ground
	startingPosition
)

func fs1(input io.Reader) int {
	lines := aoc.ReaderToStrings(input)
	board, start := toBoard(lines)

	next, dir := getNextPosition(board, start)
	distance := 1
	for next != start {
		c := board[next]
		switch c {
		case horizontal:
			switch dir {
			case aoc.Left, aoc.Right:
				next = next.Move(dir, 1)
			default:
				panic(dir)
			}
		case vertical:
			switch dir {
			case aoc.Up, aoc.Down:
				next = next.Move(dir, 1)
			default:
				panic(dir)
			}
		case bendL:
			switch dir {
			case aoc.Down:
				dir = aoc.Right
			case aoc.Left:
				dir = aoc.Up
			default:
				panic(dir)
			}
			next = next.Move(dir, 1)
		case bendJ:
			switch dir {
			case aoc.Right:
				dir = aoc.Up
			case aoc.Down:
				dir = aoc.Left
			default:
				panic(dir)
			}
			next = next.Move(dir, 1)
		case bend7:
			switch dir {
			case aoc.Right:
				dir = aoc.Down
			case aoc.Up:
				dir = aoc.Left
			default:
				panic(dir)
			}
			next = next.Move(dir, 1)
		case bendF:
			switch dir {
			case aoc.Up:
				dir = aoc.Right
			case aoc.Left:
				dir = aoc.Down
			default:
				panic(dir)
			}
			next = next.Move(dir, 1)

		default:
			panic(c)
		}
		distance++
	}

	return (distance + 1) / 2
}

func toBoard(lines []string) (map[aoc.Position]tileType, aoc.Position) {
	board := make(map[aoc.Position]tileType)
	var start aoc.Position

	for row, line := range lines {
		for col, c := range line {
			pos := aoc.Position{Row: row, Col: col}
			switch c {
			case '|':
				board[pos] = vertical
			case '-':
				board[pos] = horizontal
			case 'L':
				board[pos] = bendL
			case 'J':
				board[pos] = bendJ
			case '7':
				board[pos] = bend7
			case 'F':
				board[pos] = bendF
			case '.':
				board[pos] = ground
			case 'S':
				board[pos] = startingPosition
				start = pos
			default:
				panic(line)
			}
		}
	}

	return board, start
}

func getNextPosition(board map[aoc.Position]tileType, start aoc.Position) (aoc.Position, aoc.Direction) {
	var (
		tileUp    = board[start.Delta(-1, 0)]
		tileDown  = board[start.Delta(1, 0)]
		tileLeft  = board[start.Delta(0, -1)]
		tileRight = board[start.Delta(0, 1)]
	)

	switch tileUp {
	case bend7, bendF, vertical:
		return start.Delta(-1, 0), aoc.Up
	}
	switch tileDown {
	case bendL, bendJ, vertical:
		return start.Delta(1, 0), aoc.Down
	}
	switch tileLeft {
	case bendL, bendF, horizontal:
		return start.Delta(0, -1), aoc.Left
	}
	switch tileRight {
	case bendJ, bend7, horizontal:
		return start.Delta(0, 1), aoc.Right
	}
	panic("direction")
}

func fs2(input io.Reader) int {
	lines := aoc.ReaderToStrings(input)
	board, start := toBoard(lines)

	loopPositions := make(map[aoc.Position]bool)
	loopPositions[start] = true

	next, dir := getNextPosition(board, start)
	for next != start {
		loopPositions[next] = true
		c := board[next]
		switch c {
		case horizontal:
			switch dir {
			case aoc.Left, aoc.Right:
				next = next.Move(dir, 1)
			default:
				panic(dir)
			}
		case vertical:
			switch dir {
			case aoc.Up, aoc.Down:
				next = next.Move(dir, 1)
			default:
				panic(dir)
			}
		case bendL:
			switch dir {
			case aoc.Down:
				dir = aoc.Right
			case aoc.Left:
				dir = aoc.Up
			default:
				panic(dir)
			}
			next = next.Move(dir, 1)
		case bendJ:
			switch dir {
			case aoc.Right:
				dir = aoc.Up
			case aoc.Down:
				dir = aoc.Left
			default:
				panic(dir)
			}
			next = next.Move(dir, 1)
		case bend7:
			switch dir {
			case aoc.Right:
				dir = aoc.Down
			case aoc.Up:
				dir = aoc.Left
			default:
				panic(dir)
			}
			next = next.Move(dir, 1)
		case bendF:
			switch dir {
			case aoc.Up:
				dir = aoc.Right
			case aoc.Left:
				dir = aoc.Down
			default:
				panic(dir)
			}
			next = next.Move(dir, 1)

		default:
			panic(c)
		}
	}

	res := 0
	for pos := range board {
		if loopPositions[pos] {
			continue
		}
		if board[pos] != ground {
			continue
		}
		if isInsideLoop(board, loopPositions, pos) {
			res++
		}
	}
	return res
}

func isInsideLoop(board map[aoc.Position]tileType, loopPositions map[aoc.Position]bool, start aoc.Position) bool {
	q := []aoc.Position{start}
	visited := make(map[aoc.Position]bool)
	for len(q) != 0 {
		pos := q[0]
		q = q[1:]
		if visited[pos] {
			continue
		}
		if board[pos] == none {
			return false
		}
		if loopPositions[pos] {
			continue
		}
		visited[pos] = true
		q = append(q, pos.Move(aoc.Left, 1))
		q = append(q, pos.Move(aoc.Right, 1))
		q = append(q, pos.Move(aoc.Up, 1))
		q = append(q, pos.Move(aoc.Down, 1))
		q = append(q, pos.Move(aoc.UpLeft, 1))
		q = append(q, pos.Move(aoc.UpRight, 1))
		q = append(q, pos.Move(aoc.DownLeft, 1))
		q = append(q, pos.Move(aoc.DownRight, 1))
	}
	fmt.Println(start)
	return true
}

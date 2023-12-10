package main

import (
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
	inside
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
				panic(string(c))
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
	replaceStart(board, start)

	count := 0
	for row := 0; row < len(lines); row++ {
		for col := 0; col < len(lines[0]); col++ {
			pos := aoc.Position{Row: row, Col: col}
			if isInsideHorizontal(board, loopPositions, pos, aoc.Right) {
				board[pos] = inside
				count++
			}
		}
	}

	return count
}

func replaceStart(board map[aoc.Position]tileType, start aoc.Position) {
	var (
		tileUp    = board[start.Delta(-1, 0)]
		tileDown  = board[start.Delta(1, 0)]
		tileLeft  = board[start.Delta(0, -1)]
		tileRight = board[start.Delta(0, 1)]
		up        = false
		down      = false
		left      = false
		right     = false
	)

	switch tileUp {
	case bend7, bendF, vertical:
		up = true
	}
	switch tileDown {
	case bendL, bendJ, vertical:
		down = true
	}
	switch tileLeft {
	case bendL, bendF, horizontal:
		left = true
	}
	switch tileRight {
	case bendJ, bend7, horizontal:
		right = true
	}

	var t tileType
	if up {
		if down {
			t = vertical
		} else if left {
			t = bendJ
		} else if right {
			t = bendL
		}
	} else if down {
		if up {
			t = vertical
		} else if left {
			t = bend7
		} else if right {
			t = bendF
		}
	} else if left {
		if up {
			t = bendJ
		} else if down {
			t = bend7
		} else if right {
			t = horizontal
		}
	}
	if t == none {
		panic(start)
	}
	board[start] = t
}

func isInsideHorizontal(board map[aoc.Position]tileType, loopPositions map[aoc.Position]bool, pos aoc.Position, dir aoc.Direction) bool {
	if loopPositions[pos] {
		return false
	}

	count := 0
	latestFigure := none
	for {
		pos = pos.Move(dir, 1)
		if board[pos] == none {
			return count%2 != 0
		}

		if loopPositions[pos] {
			switch board[pos] {
			case bendL, bendF, bendJ, bend7:
				if latestFigure == none {
					count++
					latestFigure = board[pos]
				} else {
					switch latestFigure {
					case bendL:
						// A 7 cancels an L
						if board[pos] != bend7 {
							count++
						}
					case bendF:
						// A J cancel an F
						if board[pos] != bendJ {
							count++
						}
					}
					latestFigure = none
				}
			case vertical:
				count++
			case horizontal:
			default:
				panic(pos)
			}
		}
	}
}

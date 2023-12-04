package aoc

import "fmt"

// Direction enum
type Direction int

const (
	Up Direction = iota
	Down
	Left
	Right

	UpLeft
	UpRight
	DownLeft
	DownRight
)

// Opposite returns the opposite direction.
func (d Direction) Opposite() Direction {
	switch d {
	case Up:
		return Down
	case Down:
		return Up
	case Left:
		return Right
	case Right:
		return Left
	case UpLeft:
		return DownRight
	case UpRight:
		return DownLeft
	case DownLeft:
		return UpRight
	case DownRight:
		return UpLeft
	}
	panic("not handled")
}

// Turn turns into a given direction.
func (d Direction) Turn(turn Direction) Direction {
	if turn == Up || turn == Down {
		return turn
	}

	switch d {
	case Up:
		return turn
	case Down:
		switch turn {
		case Left:
			return Right
		case Right:
			return Left
		}
	case Left:
		switch turn {
		case Left:
			return Down
		case Right:
			return Up
		}
	case Right:
		switch turn {
		case Left:
			return Up
		case Right:
			return Down
		}
	}

	panic("not handled")
}

// Position represents a given position (row/col)
type Position struct {
	Row int
	Col int
}

// String implements strings.Stringer.
func (p Position) String() string {
	return fmt.Sprintf("row=%d, col=%d", p.Row, p.Col)
}

// Manhattan returns the manhattan distance.
func (p Position) Manhattan(p2 Position) int {
	return Abs(p.Row-p2.Row) + Abs(p.Col-p2.Col)
}

// ManhattanZero returns the manhattan distance from the zero position.
func (p Position) ManhattanZero() int {
	return p.Manhattan(Position{})
}

// Delta returns a new position from a row delta and col delta.
func (p Position) Delta(row, col int) Position {
	return Position{
		Row: p.Row + row,
		Col: p.Col + col,
	}
}

// Move moves into a given direction and a certain number of times.
func (p Position) Move(direction Direction, moves int) Position {
	switch direction {
	case Up:
		return p.Delta(-moves, 0)
	case Down:
		return p.Delta(moves, 0)
	case Left:
		return p.Delta(0, -moves)
	case Right:
		return p.Delta(0, moves)
	case UpLeft:
		return p.Delta(-moves, -moves)
	case UpRight:
		return p.Delta(-moves, moves)
	case DownLeft:
		return p.Delta(moves, -moves)
	case DownRight:
		return p.Delta(moves, moves)
	}

	panic("not handled")
}

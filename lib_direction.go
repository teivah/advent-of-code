package aoc

import (
	"fmt"
)

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

// Rev reverses the current direction.
func (d Direction) Rev() Direction {
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

// Turn turns left or right.
func (d Direction) Turn(turn Direction) Direction {
	if turn != Left && turn != Right {
		panic("should be left or right")
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

// String implements strings.Stringer.
func (d Direction) String() string {
	switch d {
	case Up:
		return "up"
	case Down:
		return "down"
	case Left:
		return "left"
	case Right:
		return "right"
	case UpLeft:
		return "up left"
	case UpRight:
		return "up right"
	case DownLeft:
		return "down left"
	case DownRight:
		return "down right"
	default:
		panic(fmt.Sprintf("unknown direction %d", d))
	}
}

// Position represents a given position (row/col)
type Position struct {
	Row int
	Col int
}

// NewPosition creates a new position.
func NewPosition(row, col int) Position {
	return Position{Row: row, Col: col}
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

// Location represents a given position and direction.
type Location struct {
	Pos Position
	Dir Direction
}

// NewLocation creates a new location.
func NewLocation(row, col int, dir Direction) Location {
	return Location{
		Pos: NewPosition(row, col),
		Dir: dir,
	}
}

// Turn turns left or right.
func (l Location) Turn(d Direction, moves int) Location {
	dir := l.Dir.Turn(d)
	pos := l.Pos.Move(dir, moves)
	return Location{Pos: pos, Dir: dir}
}

// Rev moves in the reverse direction.
func (l Location) Rev(moves int) Location {
	dir := l.Dir.Rev()
	pos := l.Pos.Move(dir, moves)
	return Location{Pos: pos, Dir: dir}
}

// Straight moves in the current direction.
func (l Location) Straight(moves int) Location {
	pos := l.Pos.Move(l.Dir, moves)
	return Location{Pos: pos, Dir: l.Dir}
}

// Move moves in a given direction.
func (l Location) Move(d Direction, moves int) Location {
	return Location{Pos: l.Pos.Move(d, moves), Dir: d}
}

// String implements strings.Stringer.
func (l Location) String() string {
	return fmt.Sprintf("dir=%s, pos=%s", l.Dir, l.Pos)
}

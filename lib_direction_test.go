package aoc

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestLocation(t *testing.T) {
	l := NewLocation(1, 1, Right)
	assert.Equal(t, "dir=right, pos=row=1, col=1", l.String())
	assert.Equal(t, Location{Dir: Up, Pos: Position{Row: 0, Col: 1}}, l.Turn(Left, 1))
	assert.Equal(t, Location{Dir: Up, Pos: Position{Row: -1, Col: 1}}, l.Turn(Left, 2))
	assert.Equal(t, Location{Dir: Left, Pos: Position{Row: 1, Col: 0}}, l.Rev(1))
	assert.Equal(t, Location{Dir: Right, Pos: Position{Row: 1, Col: 2}}, l.Straight(1))
	assert.Equal(t, Location{Dir: Left, Pos: Position{Row: 1, Col: 0}}, l.Move(Left, 1))
	assert.Equal(t, Location{Dir: DownLeft, Pos: Position{Row: 2, Col: 0}}, l.Move(DownLeft, 1))

	l = NewLocation(1, 1, Left)
	assert.Equal(t, Location{Dir: Down, Pos: Position{Row: 2, Col: 1}}, l.Turn(Left, 1))
	assert.Equal(t, Location{Dir: Down, Pos: Position{Row: 3, Col: 1}}, l.Turn(Left, 2))
	assert.Equal(t, Location{Dir: Right, Pos: Position{Row: 1, Col: 2}}, l.Rev(1))
	assert.Equal(t, Location{Dir: Left, Pos: Position{Row: 1, Col: 0}}, l.Straight(1))
	assert.Equal(t, Location{Dir: Left, Pos: Position{Row: 1, Col: 0}}, l.Move(Left, 1))

	l = NewLocation(1, 1, Up)
	assert.Equal(t, Location{Dir: Left, Pos: Position{Row: 1, Col: 0}}, l.Turn(Left, 1))
	assert.Equal(t, Location{Dir: Left, Pos: Position{Row: 1, Col: -1}}, l.Turn(Left, 2))
	assert.Equal(t, Location{Dir: Down, Pos: Position{Row: 2, Col: 1}}, l.Rev(1))
	assert.Equal(t, Location{Dir: Up, Pos: Position{Row: 0, Col: 1}}, l.Straight(1))
	assert.Equal(t, Location{Dir: Left, Pos: Position{Row: 1, Col: 0}}, l.Move(Left, 1))

	l = NewLocation(1, 1, Down)
	assert.Equal(t, Location{Dir: Right, Pos: Position{Row: 1, Col: 2}}, l.Turn(Left, 1))
	assert.Equal(t, Location{Dir: Right, Pos: Position{Row: 1, Col: 3}}, l.Turn(Left, 2))
	assert.Equal(t, Location{Dir: Up, Pos: Position{Row: 0, Col: 1}}, l.Rev(1))
	assert.Equal(t, Location{Dir: Down, Pos: Position{Row: 2, Col: 1}}, l.Straight(1))
	assert.Equal(t, Location{Dir: Left, Pos: Position{Row: 1, Col: 0}}, l.Move(Left, 1))
}

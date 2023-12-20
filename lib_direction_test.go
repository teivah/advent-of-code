package aoc_test

import (
	"testing"

	"github.com/stretchr/testify/assert"
	aoc "github.com/teivah/advent-of-code"
)

func TestLocation(t *testing.T) {
	l := aoc.NewLocation(1, 1, aoc.Right)
	assert.Equal(t, "dir=right, pos=row=1, col=1", l.String())
	assert.Equal(t, aoc.Location{Dir: aoc.Up, Pos: aoc.Position{Row: 0, Col: 1}}, l.Turn(aoc.Left, 1))
	assert.Equal(t, aoc.Location{Dir: aoc.Up, Pos: aoc.Position{Row: -1, Col: 1}}, l.Turn(aoc.Left, 2))
	assert.Equal(t, aoc.Location{Dir: aoc.Left, Pos: aoc.Position{Row: 1, Col: 0}}, l.Rev(1))
	assert.Equal(t, aoc.Location{Dir: aoc.Right, Pos: aoc.Position{Row: 1, Col: 2}}, l.Straight(1))
	assert.Equal(t, aoc.Location{Dir: aoc.Left, Pos: aoc.Position{Row: 1, Col: 0}}, l.Move(aoc.Left, 1))
	assert.Equal(t, aoc.Location{Dir: aoc.DownLeft, Pos: aoc.Position{Row: 2, Col: 0}}, l.Move(aoc.DownLeft, 1))

	l = aoc.NewLocation(1, 1, aoc.Left)
	assert.Equal(t, aoc.Location{Dir: aoc.Down, Pos: aoc.Position{Row: 2, Col: 1}}, l.Turn(aoc.Left, 1))
	assert.Equal(t, aoc.Location{Dir: aoc.Down, Pos: aoc.Position{Row: 3, Col: 1}}, l.Turn(aoc.Left, 2))
	assert.Equal(t, aoc.Location{Dir: aoc.Right, Pos: aoc.Position{Row: 1, Col: 2}}, l.Rev(1))
	assert.Equal(t, aoc.Location{Dir: aoc.Left, Pos: aoc.Position{Row: 1, Col: 0}}, l.Straight(1))
	assert.Equal(t, aoc.Location{Dir: aoc.Left, Pos: aoc.Position{Row: 1, Col: 0}}, l.Move(aoc.Left, 1))

	l = aoc.NewLocation(1, 1, aoc.Up)
	assert.Equal(t, aoc.Location{Dir: aoc.Left, Pos: aoc.Position{Row: 1, Col: 0}}, l.Turn(aoc.Left, 1))
	assert.Equal(t, aoc.Location{Dir: aoc.Left, Pos: aoc.Position{Row: 1, Col: -1}}, l.Turn(aoc.Left, 2))
	assert.Equal(t, aoc.Location{Dir: aoc.Down, Pos: aoc.Position{Row: 2, Col: 1}}, l.Rev(1))
	assert.Equal(t, aoc.Location{Dir: aoc.Up, Pos: aoc.Position{Row: 0, Col: 1}}, l.Straight(1))
	assert.Equal(t, aoc.Location{Dir: aoc.Left, Pos: aoc.Position{Row: 1, Col: 0}}, l.Move(aoc.Left, 1))

	l = aoc.NewLocation(1, 1, aoc.Down)
	assert.Equal(t, aoc.Location{Dir: aoc.Right, Pos: aoc.Position{Row: 1, Col: 2}}, l.Turn(aoc.Left, 1))
	assert.Equal(t, aoc.Location{Dir: aoc.Right, Pos: aoc.Position{Row: 1, Col: 3}}, l.Turn(aoc.Left, 2))
	assert.Equal(t, aoc.Location{Dir: aoc.Up, Pos: aoc.Position{Row: 0, Col: 1}}, l.Rev(1))
	assert.Equal(t, aoc.Location{Dir: aoc.Down, Pos: aoc.Position{Row: 2, Col: 1}}, l.Straight(1))
	assert.Equal(t, aoc.Location{Dir: aoc.Left, Pos: aoc.Position{Row: 1, Col: 0}}, l.Move(aoc.Left, 1))
}

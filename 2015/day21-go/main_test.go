package main

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestWin(t *testing.T) {
	win(8, 5, 5, 12, 7, 2)
}

func TestFs1Input(t *testing.T) {
	v := fs1(104, 8, 1, []Unit{
		{cost: 8, damage: 4, armor: 0},
		{cost: 10, damage: 5, armor: 0},
		{cost: 25, damage: 6, armor: 0},
		{cost: 40, damage: 7, armor: 0},
		{cost: 74, damage: 8, armor: 0},
	}, []Unit{
		{cost: 13, damage: 0, armor: 1},
		{cost: 31, damage: 0, armor: 2},
		{cost: 53, damage: 0, armor: 3},
		{cost: 75, damage: 0, armor: 4},
		{cost: 102, damage: 0, armor: 5},
	}, []Unit{
		{cost: 25, damage: 1, armor: 0},
		{cost: 50, damage: 2, armor: 0},
		{cost: 100, damage: 3, armor: 0},
		{cost: 20, damage: 0, armor: 1},
		{cost: 40, damage: 0, armor: 2},
		{cost: 80, damage: 0, armor: 3},
	})
	assert.Equal(t, 42, v)
}

func TestFs2Input(t *testing.T) {
	v := fs2(104, 8, 1, []Unit{
		{cost: 8, damage: 4, armor: 0},
		{cost: 10, damage: 5, armor: 0},
		{cost: 25, damage: 6, armor: 0},
		{cost: 40, damage: 7, armor: 0},
		{cost: 74, damage: 8, armor: 0},
	}, []Unit{
		{cost: 13, damage: 0, armor: 1},
		{cost: 31, damage: 0, armor: 2},
		{cost: 53, damage: 0, armor: 3},
		{cost: 75, damage: 0, armor: 4},
		{cost: 102, damage: 0, armor: 5},
	}, []Unit{
		{cost: 25, damage: 1, armor: 0},
		{cost: 50, damage: 2, armor: 0},
		{cost: 100, damage: 3, armor: 0},
		{cost: 20, damage: 0, armor: 1},
		{cost: 40, damage: 0, armor: 2},
		{cost: 80, damage: 0, armor: 3},
	})
	assert.Equal(t, 42, v)
}

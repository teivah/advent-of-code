package main

import (
	"fmt"
	"os"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestFs1Test(t *testing.T) {
	f, err := os.Open("test.txt")
	require.NoError(t, err)
	assert.Equal(t, 5587, fs1(f, 10000))
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 42, fs1(f, 10000))
}

func TestFs2Test(t *testing.T) {
	f, err := os.Open("test.txt")
	require.NoError(t, err)
	assert.Equal(t, 2511944, fs2(f, 10000000))
}

func TestFs2Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 42, fs2(f, 10000000))
}

func TestGrid_move(t *testing.T) {
	g := &Grid{
		infected:   [][]bool{{false, false}, {false, false}},
		row:        1,
		col:        1,
		heading:    Down,
		infections: 0,
	}
	g.move()
	fmt.Println(g)
}

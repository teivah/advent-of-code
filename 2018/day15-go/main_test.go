package main

import (
	"os"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestUnit1(t *testing.T) {
	assert.Equal(t, 18740, fs1(strings.NewReader(`#########   
#G......#
#.E.#...#
#..##..G#
#...##..#   
#...#...#
#.G...G.#   
#.....G.#   
#########`)))
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 264384, fs1(f))
}

func TestFs2Test(t *testing.T) {
	assert.Equal(t, 4988, fs2(strings.NewReader(`#######
#.G...#
#...EG#
#.#.#G#
#..G#E#
#.....#
#######`)))
}

func TestFs2Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 42, fs2(f))
}

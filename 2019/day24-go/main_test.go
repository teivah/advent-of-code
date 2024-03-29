package main

import (
	"os"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	lib "github.com/teivah/advent-of-code"
)

func TestBiodiversityRating(t *testing.T) {
	grid := toGrid(lib.ReaderToStrings(strings.NewReader(`.....
.....
.....
#....
.#...`)))
	assert.Equal(t, 2129920, grid.biodiversityRating())
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 32506911, fs1(f))
}

func TestFs2Test(t *testing.T) {
	f, err := os.Open("test.txt")
	require.NoError(t, err)
	assert.Equal(t, 99, fs2(f, 10))
}

func TestFs2Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 2025, fs2(f, 200))
}

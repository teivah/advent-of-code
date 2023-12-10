package main

import (
	"os"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestFs1Test1(t *testing.T) {
	assert.Equal(t, 4, fs1(strings.NewReader(`-L|F7
7S-7|
L|7||
-L-J|
L|-JF`)))
}

func TestFs1Test2(t *testing.T) {
	assert.Equal(t, 8, fs1(strings.NewReader(`7-F7-
.FJ|7
SJLL7
|F--J
LJ.LJ`)))
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 6882, fs1(f))
}

func TestFs2Test1(t *testing.T) {
	assert.Equal(t, 4, fs2(strings.NewReader(`...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
...........`)))
}

func TestFs2Test2(t *testing.T) {
	assert.Equal(t, 8, fs2(strings.NewReader(`.F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ...`)))
}

func TestFs2Test3(t *testing.T) {
	assert.Equal(t, 10, fs2(strings.NewReader(`FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L`)))
}

func TestFs2Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 752, fs2(f))
}

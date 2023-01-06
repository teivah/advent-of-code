package main

import (
	"os"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestFs1Test(t *testing.T) {
	f, err := os.Open("test.txt")
	require.NoError(t, err)
	v, err := fs1(f)
	require.NoError(t, err)
	assert.Equal(t, 6032, v)
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	v, err := fs1(f)
	require.NoError(t, err)
	assert.Equal(t, 75254, v)
}

func TestFs2Test(t *testing.T) {
	f, err := os.Open("test.txt")
	require.NoError(t, err)
	v, err := fs2(f, 4, func(cubeSize int, faces map[int]Face, cube [][][]*Position, board [][]Position) {
		// 1
		for row := 0; row < cubeSize; row++ {
			for col := 0; col < cubeSize; col++ {
				r := faces[1].row + row
				c := faces[1].col + col
				x := col + 1
				y := 0
				z := cubeSize - row
				cube[x][y][z] = &board[r][c]
			}
		}
		// 2
		for row := 0; row < cubeSize; row++ {
			for col := 0; col < cubeSize; col++ {
				r := faces[2].row + row
				c := faces[2].col + col
				x := cubeSize - col
				y := row + 1
				z := cubeSize + 1
				cube[x][y][z] = &board[r][c]
			}
		}
		// 3
		for row := 0; row < cubeSize; row++ {
			for col := 0; col < cubeSize; col++ {
				r := faces[3].row + row
				c := faces[3].col + col
				x := 0
				y := row + 1
				z := cubeSize - col
				cube[x][y][z] = &board[r][c]
			}
		}
		// 4
		for row := 0; row < cubeSize; row++ {
			for col := 0; col < cubeSize; col++ {
				r := faces[4].row + row
				c := faces[4].col + col
				x := col + 1
				y := row + 1
				z := 0
				cube[x][y][z] = &board[r][c]
			}
		}
		// 5
		for row := 0; row < cubeSize; row++ {
			for col := 0; col < cubeSize; col++ {
				r := faces[5].row + row
				c := faces[5].col + col
				x := col + 1
				y := cubeSize + 1
				z := row + 1
				cube[x][y][z] = &board[r][c]
			}
		}
		// 6
		for row := 0; row < cubeSize; row++ {
			for col := 0; col < cubeSize; col++ {
				r := faces[6].row + row
				c := faces[6].col + col
				x := cubeSize + 1
				y := cubeSize - col
				z := row + 1
				cube[x][y][z] = &board[r][c]
			}
		}
	})
	require.NoError(t, err)
	assert.Equal(t, 5031, v)
}

func TestFs2Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	v, err := fs2(f, 50, func(cubeSize int, faces map[int]Face, cube [][][]*Position, board [][]Position) {
		// 1
		for row := 0; row < cubeSize; row++ {
			for col := 0; col < cubeSize; col++ {
				r := faces[1].row + row
				c := faces[1].col + col
				x := col + 1
				y := 0
				z := cubeSize - row
				cube[x][y][z] = &board[r][c]
			}
		}
		// 2
		for row := 0; row < cubeSize; row++ {
			for col := 0; col < cubeSize; col++ {
				r := faces[6].row + row
				c := faces[6].col + col
				x := row + 1
				y := col + 1
				z := cubeSize + 1
				cube[x][y][z] = &board[r][c]
			}
		}
		// 3
		for row := 0; row < cubeSize; row++ {
			for col := 0; col < cubeSize; col++ {
				r := faces[4].row + row
				c := faces[4].col + col
				x := 0
				y := col + 1
				z := row + 1
				cube[x][y][z] = &board[r][c]
			}
		}
		// 4
		for row := 0; row < cubeSize; row++ {
			for col := 0; col < cubeSize; col++ {
				r := faces[3].row + row
				c := faces[3].col + col
				x := col + 1
				y := row + 1
				z := 0
				cube[x][y][z] = &board[r][c]
			}
		}
		// 5
		for row := 0; row < cubeSize; row++ {
			for col := 0; col < cubeSize; col++ {
				r := faces[5].row + row
				c := faces[5].col + col
				x := col + 1
				y := cubeSize + 1
				z := row + 1
				cube[x][y][z] = &board[r][c]
			}
		}
		// 6
		for row := 0; row < cubeSize; row++ {
			for col := 0; col < cubeSize; col++ {
				r := faces[2].row + row
				c := faces[2].col + col
				x := cubeSize + 1
				y := col + 1
				z := cubeSize - row
				cube[x][y][z] = &board[r][c]
			}
		}
	})
	require.NoError(t, err)
	assert.Equal(t, 108311, v)
}

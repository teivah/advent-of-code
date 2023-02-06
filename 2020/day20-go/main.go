package main

import (
	"bufio"
	"io"
	"math"

	aoc "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	tiles := toTiles(aoc.ReaderToStrings(input))
	squareLength := int(math.Sqrt(float64(len(tiles))))
	squares := make([][]SelectedTile, squareLength)
	for i := 0; i < squareLength; i++ {
		squares[i] = make([]SelectedTile, squareLength)
	}
	return buildSquare(tiles, squareLength, 0, 0, squares)
}

func buildSquare(tiles map[int]Tile, squareLength int, row, col int, squares [][]SelectedTile) int {
	if row == squareLength {
		return squares[0][0].id *
			squares[0][squareLength-1].id *
			squares[squareLength-1][0].id *
			squares[squareLength-1][squareLength-1].id
	}

	cpy := copyTiles(tiles)
	for id, tile := range tiles {
		hash := tile.hashes[0]
		//for _, hash := range tile.hashes {
		// Up
		if row != 0 {
			if hash[aoc.Up] != squares[row-1][col].hashes[aoc.Down] {
				continue
			}
		}

		// Left
		if col != 0 {
			if hash[aoc.Left] != squares[row][col-1].hashes[aoc.Right] {
				continue
			}
		}

		delete(cpy, id)
		squares[row][col] = SelectedTile{
			id:     id,
			hashes: hash,
		}
		nextRow := row
		nextCol := col + 1
		if nextCol == squareLength {
			nextCol = 0
			nextRow++
		}
		if v := buildSquare(cpy, squareLength, nextRow, nextCol, squares); v != -1 {
			return v
		}
		cpy[id] = tile
		//}
	}
	return -1
}

func copyTiles(tiles map[int]Tile) map[int]Tile {
	res := make(map[int]Tile, len(tiles))
	for k, v := range tiles {
		res[k] = v
	}
	return res
}

func toTiles(lines []string) map[int]Tile {
	tiles := make(map[int]Tile)

	for i := 0; i < len(lines); i++ {
		id := aoc.StringToInt(lines[i][5 : len(lines[i])-1])
		i++

		var board [][]bool
		for ; i < len(lines); i++ {
			if lines[i] == "" {
				break
			}

			var row []bool
			for j := 0; j < len(lines[j]); j++ {
				if lines[i][j] == '#' {
					row = append(row, true)
				} else {
					row = append(row, false)
				}
			}
			board = append(board, row)
		}

		tiles[id] = Tile{
			id:     id,
			board:  board,
			hashes: computeHashes(board),
		}
	}

	return tiles
}

func computeHashes(board [][]bool) []map[aoc.Direction]int {
	up := 0
	down := 0
	for col := 0; col < len(board[0]); col++ {
		if board[0][col] {
			up += 1 << col
		}

		if board[len(board)-1][col] {
			down += 1 << col
		}
	}

	left := 0
	right := 0
	for row := 0; row < len(board); row++ {
		if board[row][0] {
			left += 1 << row
		}
		if board[row][len(board[0])-1] {
			right += 1 << row
		}
	}

	a := up
	b := left
	c := down
	d := right
	return []map[aoc.Direction]int{
		{
			aoc.Up:    a,
			aoc.Left:  b,
			aoc.Down:  c,
			aoc.Right: d,
		},
		{
			aoc.Up:    flip(b),
			aoc.Left:  c,
			aoc.Down:  flip(d),
			aoc.Right: a,
		},
		{
			aoc.Up:    flip(c),
			aoc.Left:  flip(d),
			aoc.Down:  flip(a),
			aoc.Right: flip(b),
		},
		{
			aoc.Up:    d,
			aoc.Left:  flip(a),
			aoc.Down:  c,
			aoc.Right: flip(b),
		},
		{
			aoc.Up:    a,
			aoc.Left:  d,
			aoc.Down:  c,
			aoc.Right: b,
		},
		{
			aoc.Up:    flip(b),
			aoc.Left:  a,
			aoc.Down:  flip(d),
			aoc.Right: c,
		},
		{
			aoc.Up:    flip(c),
			aoc.Left:  flip(b),
			aoc.Down:  flip(a),
			aoc.Right: flip(d),
		},
		{
			aoc.Up:    d,
			aoc.Left:  flip(c),
			aoc.Down:  b,
			aoc.Right: flip(a),
		},
	}
}

func flip(x int) int {
	flipped := 0
	for i := 0; i < 9; i++ {
		flipped = flipped << 1
		if x&1 == 1 {
			flipped = flipped | 1
		}
		x = x >> 1
	}
	return flipped
}

type SelectedTile struct {
	id     int
	hashes map[aoc.Direction]int
}

type Tile struct {
	id     int
	board  [][]bool
	hashes []map[aoc.Direction]int
}

func rotateHorizontally(matrix [][]bool) [][]bool {
	rows := len(matrix)
	cols := len(matrix[0])
	flipped := make([][]bool, rows)
	for i := range flipped {
		flipped[i] = make([]bool, cols)
	}

	for i := 0; i < rows; i++ {
		for j := 0; j < cols; j++ {
			flipped[i][j] = matrix[i][cols-j-1]
		}
	}

	return flipped
}

func rotateVertically(matrix [][]bool) [][]bool {
	rows := len(matrix)
	cols := len(matrix[0])
	flipped := make([][]bool, rows)
	for i := range flipped {
		flipped[i] = make([]bool, cols)
	}

	for i := 0; i < rows; i++ {
		for j := 0; j < cols; j++ {
			flipped[i][j] = matrix[rows-i-1][j]
		}
	}

	return flipped
}

func fs2(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
		line := scanner.Text()
		_ = line
	}

	return 42
}

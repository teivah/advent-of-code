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
		for _, hash := range tile.hashes {
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
		}
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

		var hashes []map[aoc.Direction]int
		hashes = append(hashes, computeHashes(board))
		hashes = append(hashes, computeHashes(flipMatrix(board)))
		hashes = append(hashes, computeHashes(rotateMatrix(board)))
		hashes = append(hashes, computeHashes(flipMatrix(rotateMatrix(board))))
		hashes = append(hashes, computeHashes(rotateMatrix(rotateMatrix(board))))
		hashes = append(hashes, computeHashes(flipMatrix(rotateMatrix(rotateMatrix(board)))))
		hashes = append(hashes, computeHashes(rotateMatrix(rotateMatrix(rotateMatrix(board)))))
		hashes = append(hashes, computeHashes(flipMatrix(rotateMatrix(rotateMatrix(rotateMatrix(board))))))

		tiles[id] = Tile{
			id:     id,
			board:  board,
			hashes: hashes,
		}
	}

	return tiles
}

func computeHashes(board [][]bool) map[aoc.Direction]int {
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

	return map[aoc.Direction]int{
		aoc.Up:    up,
		aoc.Left:  left,
		aoc.Down:  down,
		aoc.Right: right,
	}
}

func flipMatrix(matrix [][]bool) [][]bool {
	n := len(matrix)
	m := len(matrix[0])

	flipped := make([][]bool, n)
	for i := range flipped {
		flipped[i] = make([]bool, m)
	}

	for i := 0; i < n; i++ {
		for j := 0; j < m; j++ {
			flipped[n-i-1][j] = matrix[i][j]
		}
	}

	return flipped
}

func rotateMatrix(matrix [][]bool) [][]bool {
	n := len(matrix)
	m := len(matrix[0])

	flipped := make([][]bool, m)
	for i := range flipped {
		flipped[i] = make([]bool, n)
	}

	for i := 0; i < n; i++ {
		for j := 0; j < m; j++ {
			flipped[j][n-i-1] = matrix[i][j]
		}
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

func fs2(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
		line := scanner.Text()
		_ = line
	}

	return 42
}

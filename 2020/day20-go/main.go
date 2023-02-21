package main

import (
	"io"
	"math"

	aoc "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	tiles, _ := toTiles(aoc.ReaderToStrings(input))
	squareLength := int(math.Sqrt(float64(len(tiles))))
	squares := make([][]SelectedTile, squareLength)
	for i := 0; i < squareLength; i++ {
		squares[i] = make([]SelectedTile, squareLength)
	}
	v, _ := buildSquare(tiles, squareLength, 0, 0, squares)
	return v
}

func buildSquare(tiles map[int][]Tile, squareLength int, row, col int, squares [][]SelectedTile) (int, [][]SelectedTile) {
	if row == squareLength {
		return squares[0][0].tileID *
			squares[0][squareLength-1].tileID *
			squares[squareLength-1][0].tileID *
			squares[squareLength-1][squareLength-1].tileID, squares
	}

	cpy := copyTiles(tiles)
	for id, subtiles := range tiles {
		for transformID, tile := range subtiles {
			// Up
			if row != 0 {
				if tile.hashes[aoc.Up] != squares[row-1][col].hashes[aoc.Down] {
					continue
				}
			}

			// Left
			if col != 0 {
				if tile.hashes[aoc.Left] != squares[row][col-1].hashes[aoc.Right] {
					continue
				}
			}

			delete(cpy, id)
			squares[row][col] = SelectedTile{
				tileID:      id,
				transformID: transformID,
				hashes:      tile.hashes,
			}
			nextRow := row
			nextCol := col + 1
			if nextCol == squareLength {
				nextCol = 0
				nextRow++
			}
			if v, squares := buildSquare(cpy, squareLength, nextRow, nextCol, squares); v != -1 {
				return v, squares
			}
			cpy[id] = subtiles
		}
	}
	return -1, nil
}

func copyTiles(tiles map[int][]Tile) map[int][]Tile {
	res := make(map[int][]Tile, len(tiles))
	for k, v := range tiles {
		s := make([]Tile, len(v))
		for i, x := range v {
			s[i] = x
		}
		res[k] = s
	}
	return res
}

func toTiles(lines []string) (map[int][]Tile, int) {
	tiles := make(map[int][]Tile)

	length := 0
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

		tiles[id] = append(tiles[id], Tile{
			id:     id,
			board:  board,
			hashes: computeHashes(board),
		})

		b := flipMatrix(board)
		tiles[id] = append(tiles[id], Tile{
			id:     id,
			board:  b,
			hashes: computeHashes(b),
		})

		b = rotateMatrix(board)
		tiles[id] = append(tiles[id], Tile{
			id:     id,
			board:  b,
			hashes: computeHashes(b),
		})

		b = flipMatrix(rotateMatrix(board))
		tiles[id] = append(tiles[id], Tile{
			id:     id,
			board:  b,
			hashes: computeHashes(b),
		})

		b = rotateMatrix(rotateMatrix(board))
		tiles[id] = append(tiles[id], Tile{
			id:     id,
			board:  b,
			hashes: computeHashes(b),
		})

		b = flipMatrix(rotateMatrix(rotateMatrix(board)))
		tiles[id] = append(tiles[id], Tile{
			id:     id,
			board:  b,
			hashes: computeHashes(b),
		})

		b = rotateMatrix(rotateMatrix(rotateMatrix(board)))
		tiles[id] = append(tiles[id], Tile{
			id:     id,
			board:  b,
			hashes: computeHashes(b),
		})

		b = flipMatrix(rotateMatrix(rotateMatrix(rotateMatrix(board))))
		tiles[id] = append(tiles[id], Tile{
			id:     id,
			board:  b,
			hashes: computeHashes(b),
		})

		length = len(board)
	}

	return tiles, length
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
	tileID      int
	transformID int
	hashes      map[aoc.Direction]int
}

type Tile struct {
	id     int
	board  [][]bool
	hashes map[aoc.Direction]int
}

func fs2(input io.Reader) int {
	tiles, length := toTiles(aoc.ReaderToStrings(input))
	squareLength := int(math.Sqrt(float64(len(tiles))))
	squares := make([][]SelectedTile, squareLength)
	for i := 0; i < squareLength; i++ {
		squares[i] = make([]SelectedTile, squareLength)
	}

	_, selected := buildSquare(tiles, squareLength, 0, 0, squares)

	cutLength := length - 2
	final := make([][]bool, cutLength*squareLength)
	for i := 0; i < cutLength*squareLength; i++ {
		final[i] = make([]bool, cutLength*squareLength)
	}

	for r, row := range selected {
		for c, st := range row {
			tile := tiles[st.tileID][st.transformID]

			for br := 1; br < len(tile.board)-1; br++ {
				for bc := 1; bc < len(tile.board)-1; bc++ {
					final[r*cutLength+br-1][c*cutLength+bc-1] = tile.board[br][bc]
				}
			}
		}
	}

	if v := findSeaMonster(final); v != 0 {
		return v
	}

	if v := findSeaMonster(flipMatrix(final)); v != 0 {
		return v
	}

	if v := findSeaMonster(rotateMatrix(final)); v != 0 {
		return v
	}

	if v := findSeaMonster(flipMatrix(rotateMatrix(final))); v != 0 {
		return v
	}

	if v := findSeaMonster(rotateMatrix(rotateMatrix(final))); v != 0 {
		return v
	}

	if v := findSeaMonster(flipMatrix(rotateMatrix(rotateMatrix(final)))); v != 0 {
		return v
	}

	if v := findSeaMonster(rotateMatrix(rotateMatrix(rotateMatrix(final)))); v != 0 {
		return v
	}

	if v := findSeaMonster(flipMatrix(rotateMatrix(rotateMatrix(rotateMatrix(final))))); v != 0 {
		return v
	}

	return 0
}

func findSeaMonster(board [][]bool) int {
	found := false
	for row := 0; row < len(board)-2; row++ {
		for col := 0; col < len(board[0])-19; col++ {
			if board[row][col+18] &&
				board[row+1][col] &&
				board[row+1][col+5] &&
				board[row+1][col+6] &&
				board[row+1][col+11] &&
				board[row+1][col+12] &&
				board[row+1][col+17] &&
				board[row+1][col+18] &&
				board[row+1][col+19] &&
				board[row+2][col+1] &&
				board[row+2][col+4] &&
				board[row+2][col+7] &&
				board[row+2][col+10] &&
				board[row+2][col+13] &&
				board[row+2][col+16] {
				found = true
				board[row][col+18] = false
				board[row+1][col] = false
				board[row+1][col+5] = false
				board[row+1][col+6] = false
				board[row+1][col+11] = false
				board[row+1][col+12] = false
				board[row+1][col+17] = false
				board[row+1][col+18] = false
				board[row+1][col+19] = false
				board[row+2][col+1] = false
				board[row+2][col+4] = false
				board[row+2][col+7] = false
				board[row+2][col+10] = false
				board[row+2][col+13] = false
				board[row+2][col+16] = false
			}
		}
	}
	if !found {
		return 0
	}

	sum := 0
	for row := 0; row < len(board); row++ {
		for col := 0; col < len(board[0]); col++ {
			if board[row][col] {
				sum++
			}
		}
	}
	return sum
}

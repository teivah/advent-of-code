package main

import (
	"fmt"
	"io"
	"math"

	lib "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader, maxRow, maxCol int) int {
	s := lib.ReaderToString(input)

	var layers [][][]int

	grid := newGrid(maxRow, maxCol)
	for i := 0; i < len(s); i++ {
		if i%(maxRow*maxCol) == 0 && i != 0 {
			layers = append(layers, grid)
			grid = newGrid(maxRow, maxCol)
		}

		row := (i % (maxRow * maxCol)) / maxCol
		col := (i % (maxRow * maxCol)) % maxCol
		grid[row][col] = lib.RuneToInt(rune(s[i]))
	}
	layers = append(layers, grid)

	fewestZero := math.MaxInt
	res := 0
	for _, layer := range layers {
		occurences := make(map[int]int)
		for row := 0; row < maxRow; row++ {
			for col := 0; col < maxCol; col++ {
				occurences[layer[row][col]]++
			}
		}

		if occurences[0] < fewestZero {
			res = occurences[1] * occurences[2]
			fewestZero = occurences[0]
		}
	}

	return res
}

func newGrid(maxRow, maxCol int) [][]int {
	grid := make([][]int, maxRow)
	for i := 0; i < maxRow; i++ {
		grid[i] = make([]int, maxCol)
	}
	return grid
}

func fs2(input io.Reader, maxRow, maxCol int) {
	s := lib.ReaderToString(input)

	var layers [][][]int

	grid := newGrid(maxRow, maxCol)
	for i := 0; i < len(s); i++ {
		if i%(maxRow*maxCol) == 0 && i != 0 {
			layers = append(layers, grid)
			grid = newGrid(maxRow, maxCol)
		}

		row := (i % (maxRow * maxCol)) / maxCol
		col := (i % (maxRow * maxCol)) % maxCol
		grid[row][col] = lib.RuneToInt(rune(s[i]))
	}
	layers = append(layers, grid)

	grid = newGrid(maxRow, maxCol)
	for row := 0; row < maxRow; row++ {
		for col := 0; col < maxCol; col++ {
			for _, layer := range layers {
				v := layer[row][col]
				if v == 2 {
					continue
				}
				grid[row][col] = v
				break
			}
		}
	}

	for row := 0; row < maxRow; row++ {
		for col := 0; col < maxCol; col++ {
			switch grid[row][col] {
			case 1:
				fmt.Print(".")
			case 0:
				fmt.Print(" ")
			}
		}
		fmt.Println()
	}
}

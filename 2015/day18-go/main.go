package main

import (
	"bufio"
	"fmt"
	"io"
)

func fs1(input io.Reader, steps int) (int, error) {
	scanner := bufio.NewScanner(input)
	var grid [][]bool
	for scanner.Scan() {
		line := scanner.Text()
		runes := []rune(line)
		row := make([]bool, len(runes))
		for i, rune := range runes {
			if rune == '#' {
				row[i] = true
			}
		}
		grid = append(grid, row)
	}

	lights := 0
	for i := 0; i < steps; i++ {
		grid, lights = round(grid)
	}

	return lights, nil
}

func round(grid [][]bool) ([][]bool, int) {
	res := make([][]bool, len(grid))
	for i, g := range grid {
		res[i] = make([]bool, len(g))
	}

	sum := 0
	for row := 0; row < len(grid); row++ {
		for col := 0; col < len(grid[0]); col++ {
			if grid[row][col] {
				neighbors := get(grid, row-1, col-1) +
					get(grid, row-1, col) +
					get(grid, row-1, col+1) +
					get(grid, row, col-1) +
					get(grid, row, col+1) +
					get(grid, row+1, col-1) +
					get(grid, row+1, col) +
					get(grid, row+1, col+1)
				if neighbors == 2 || neighbors == 3 {
					res[row][col] = true
					sum++
				}
			} else {
				neighbors := get(grid, row-1, col-1) +
					get(grid, row-1, col) +
					get(grid, row-1, col+1) +
					get(grid, row, col-1) +
					get(grid, row, col+1) +
					get(grid, row+1, col-1) +
					get(grid, row+1, col) +
					get(grid, row+1, col+1)
				if neighbors == 3 {
					res[row][col] = true
					sum++
				}
			}
		}
	}
	return res, sum
}

func print(grid [][]bool) {
	for _, row := range grid {
		for _, col := range row {
			if col {
				fmt.Printf("#")
			} else {
				fmt.Printf(".")
			}
		}
		fmt.Println()
	}
	fmt.Println()
}

func get(grid [][]bool, row, col int) int {
	if row < 0 || col < 0 || row >= len(grid) || col >= len(grid[0]) {
		return 0
	}
	if grid[row][col] {
		return 1
	}
	return 0
}

func fs2(input io.Reader, steps int) (int, error) {
	scanner := bufio.NewScanner(input)
	var grid [][]bool
	for scanner.Scan() {
		line := scanner.Text()
		runes := []rune(line)
		row := make([]bool, len(runes))
		for i, rune := range runes {
			if rune == '#' {
				row[i] = true
			}
		}
		grid = append(grid, row)
	}

	grid[0][0] = true
	grid[0][len(grid[0])-1] = true
	grid[len(grid)-1][0] = true
	grid[len(grid)-1][len(grid[0])-1] = true

	lights := 0
	for i := 0; i < steps; i++ {
		grid, lights = round2(grid)
	}

	return lights, nil
}

func round2(grid [][]bool) ([][]bool, int) {
	res := make([][]bool, len(grid))
	for i, g := range grid {
		res[i] = make([]bool, len(g))
	}

	sum := 0
	for row := 0; row < len(grid); row++ {
		for col := 0; col < len(grid[0]); col++ {
			if (row == 0 && col == 0) ||
				(row == 0 && col == len(grid[0])-1) ||
				(row == len(grid)-1 && col == 0) ||
				(row == len(grid)-1 && col == len(grid[0])-1) {
				res[row][col] = true
				sum++
				continue
			}

			if grid[row][col] {
				neighbors := get(grid, row-1, col-1) +
					get(grid, row-1, col) +
					get(grid, row-1, col+1) +
					get(grid, row, col-1) +
					get(grid, row, col+1) +
					get(grid, row+1, col-1) +
					get(grid, row+1, col) +
					get(grid, row+1, col+1)
				if neighbors == 2 || neighbors == 3 {
					res[row][col] = true
					sum++
				}
			} else {
				neighbors := get(grid, row-1, col-1) +
					get(grid, row-1, col) +
					get(grid, row-1, col+1) +
					get(grid, row, col-1) +
					get(grid, row, col+1) +
					get(grid, row+1, col-1) +
					get(grid, row+1, col) +
					get(grid, row+1, col+1)
				if neighbors == 3 {
					res[row][col] = true
					sum++
				}
			}
		}
	}
	return res, sum
}

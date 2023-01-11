package main

import (
	"bufio"
	"fmt"
	"io"
	"strconv"
	"strings"
)

func fs1(input io.Reader) (int, error) {
	const (
		nrows = 6
		ncols = 50
	)

	grid := make([][]bool, nrows)
	for i := range grid {
		grid[i] = make([]bool, ncols)
	}

	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
		s := scanner.Text()

		if s[1] == 'e' {
			// Rectangle
			x := strings.Index(s, "x")
			cols, err := strconv.Atoi(s[5:x])
			if err != nil {
				return 0, err
			}
			rows, err := strconv.Atoi(s[x+1:])
			if err != nil {
				return 0, err
			}

			for row := 0; row < rows; row++ {
				for col := 0; col < cols; col++ {
					grid[row][col] = true
				}
			}
		} else {
			idx := indexAll(s, " ")
			if s[7] == 'c' {
				// Rotate column
				x, err := strconv.Atoi(s[16:idx[2]])
				if err != nil {
					return 0, err
				}

				by, err := strconv.Atoi(s[idx[3]+1:])
				if err != nil {
					return 0, err
				}

				rotateCol(grid, x, by)
			} else {
				// Rotate row
				y, err := strconv.Atoi(s[13:idx[2]])
				if err != nil {
					return 0, err
				}

				by, err := strconv.Atoi(s[idx[3]+1:])
				if err != nil {
					return 0, err
				}

				rotateRow(grid, y, by)
			}
		}
	}

	sum := 0
	for row := 0; row < nrows; row++ {
		for col := 0; col < ncols; col++ {
			if grid[row][col] {
				sum++
				fmt.Printf("#")
			} else {
				fmt.Printf(".")
			}
		}
		fmt.Println()
	}

	return sum, nil
}

func rotateRow(grid [][]bool, y, by int) {
	row := copyRow(grid[y])
	for i := 0; i < len(row); i++ {
		row[i] = grid[y][mod(i-by, len(row))]
	}
	grid[y] = row
}

func copyRow(s []bool) []bool {
	res := make([]bool, len(s))
	for i, v := range s {
		res[i] = v
	}
	return res
}

func rotateCol(grid [][]bool, x, by int) {
	col := copyCol(x, grid)
	for i := 0; i < len(grid); i++ {
		col[i] = grid[mod(i-by, len(grid))][x]
	}
	for i := 0; i < len(grid); i++ {
		grid[i][x] = col[i]
	}
}

func copyCol(col int, grid [][]bool) []bool {
	res := make([]bool, len(grid))
	for i := 0; i < len(grid); i++ {
		res[i] = grid[i][col]
	}
	return res
}

func mod(d, m int) int {
	res := d % m
	if (res < 0 && m > 0) || (res > 0 && m < 0) {
		return res + m
	}
	return res
}

func indexAll(s string, search string) []int {
	i := 0
	var res []int
	for i < len(s) {
		index := strings.Index(s[i:], search)
		if index == -1 {
			return res
		}
		res = append(res, index+i)
		i += index + len(search)
	}
	return res
}

func fs2(input io.Reader) (int, error) {
	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
		line := scanner.Text()
		_ = line
	}

	return 42, nil
}

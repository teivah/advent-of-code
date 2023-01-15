package main

import (
	"bufio"
	"io"
)

func fs1(input io.Reader, rows int) int {
	scanner := bufio.NewScanner(input)
	scanner.Scan()
	s := scanner.Text()
	cols := len(s)
	grid := make([][]bool, rows)
	for row := 0; row < rows; row++ {
		grid[row] = make([]bool, cols)
	}

	sum := 0
	for col := 0; col < cols; col++ {
		if s[col] == '.' {
			grid[0][col] = true
			sum++
		}
	}

	for row := 1; row < rows; row++ {
		for col := 0; col < cols; col++ {
			v := trap(grid[row-1], col)
			if !v {
				grid[row][col] = true
				sum++
			}
		}
	}

	return sum
}

func trap(g []bool, col int) bool {
	left := true
	center := g[col]
	right := true
	if col > 0 {
		left = g[col-1]
	}
	if col < len(g)-1 {
		right = g[col+1]
	}

	return (!left && !center && right) ||
		(!center && !right && left) ||
		(!left && center && right) ||
		(!right && center && left)
}

func fs2(input io.Reader, rows int) int {
	scanner := bufio.NewScanner(input)
	scanner.Scan()
	s := scanner.Text()
	cols := len(s)
	grid := make([][]bool, rows)
	for row := 0; row < rows; row++ {
		grid[row] = make([]bool, cols)
	}

	sum := 0
	for col := 0; col < cols; col++ {
		if s[col] == '.' {
			grid[0][col] = true
			sum++
		}
	}

	for row := 1; row < rows; row++ {
		for col := 0; col < cols; col++ {
			v := trap(grid[row-1], col)
			if !v {
				grid[row][col] = true
				sum++
			}
		}
	}

	//for row := 0; row < rows; row++ {
	//	for col := 0; col < cols; col++ {
	//		if grid[row][col] {
	//			fmt.Print(".")
	//		} else {
	//			fmt.Print("^")
	//		}
	//	}
	//	fmt.Println()
	//}

	return sum
}

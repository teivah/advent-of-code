package main

import (
	"bufio"
	"fmt"
	"io"

	lib "github.com/teivah/advent-of-code"
)

/*
.#.
..#
###

size % 2 == 0
=> convert each 2x2 into 3x3 using the corresponding enhancement rule

size % 3 == 0
=> convert 3x3 into 4x4

input: rotate or flip
*/
func fs1(input io.Reader, iterations int) int {
	scanner := bufio.NewScanner(input)
	options := make(map[string]string)
	for scanner.Scan() {
		line := scanner.Text()
		addOptions(line, options)
	}

	grid := [][]bool{
		{false, true, false},
		{false, false, true},
		{true, true, true},
	}

	for iteration := 0; iteration < iterations; iteration++ {
		size := len(grid)
		starting := 0
		if size%2 == 0 {
			starting = 2
		} else if size%3 == 0 {
			starting = 3
		} else {
			panic(size)
		}

		cells := len(grid) / starting
		ngrid := make([][]bool, cells*(starting+1))

		itrow := -1
		for row := 0; row < len(grid); row += starting {
			itrow++
			for col := 0; col < len(grid[0]); col += starting {
				r := -1
				c := 0
				s := ""
				for j := 0; j < starting*starting; j++ {
					if c == 0 {
						r++
					}
					if grid[row+r][col+c] {
						s += "#"
					} else {
						s += "."
					}
					c = (c + 1) % starting
				}

				if v, exists := options[s]; exists {
					nrow := itrow*(starting+1) - 1
					for i := 0; i < (starting+1)*(starting+1); i++ {
						if i%(starting+1) == 0 {
							nrow++
						}

						value := false
						if v[i] == '#' {
							value = true
						}

						ngrid[nrow] = append(ngrid[nrow], value)
					}
				} else {
					panic(s)
				}
			}
		}
		grid = ngrid

		//printGrid(grid)
	}

	sum := 0
	for _, row := range grid {
		for _, v := range row {
			if v {
				sum++
			}
		}
	}
	return sum
}

func printGrid(grid [][]bool) {
	for _, row := range grid {
		for _, v := range row {
			if v {
				fmt.Print("#")
			} else {
				fmt.Print(".")
			}
		}
		fmt.Println()
	}
	fmt.Println()
}

func addOptions(s string, options map[string]string) {
	del := lib.NewDelimiter(s, " ")

	first := del.GetString(0)
	second := del.GetString(2)

	g1 := toGrid(first)
	g2 := toGrid(second)
	s2 := toString(g2)

	options[toString(g1)] = s2

	g1 = flip(g1)
	options[toString(g1)] = s2
	g1 = flip(g1)

	g1 = rotate(g1)
	options[toString(g1)] = s2
	g1 = flip(g1)
	options[toString(g1)] = s2
	g1 = flip(g1)

	g1 = rotate(g1)
	options[toString(g1)] = s2
	g1 = flip(g1)
	options[toString(g1)] = s2
	g1 = flip(g1)

	g1 = rotate(g1)
	options[toString(g1)] = s2
	g1 = flip(g1)
	options[toString(g1)] = s2
	g1 = flip(g1)
}

func toGrid(s string) [][]bool {
	var grid [][]bool
	var cur []bool
	for i := 0; i < len(s); i++ {
		r := rune(s[i])
		switch r {
		case '#':
			cur = append(cur, true)
		case '.':
			cur = append(cur, false)
		case '/':
			grid = append(grid, cur)
			cur = make([]bool, 0)
		}
	}
	grid = append(grid, cur)
	return grid
}

func rotate(matrix [][]bool) [][]bool {
	n := len(matrix)
	m := len(matrix[0])
	result := make([][]bool, m)
	for i := range result {
		result[i] = make([]bool, n)
	}
	for i := 0; i < n; i++ {
		for j := 0; j < m; j++ {
			result[j][n-1-i] = matrix[i][j]
		}
	}
	return result
}

func flip(grid [][]bool) [][]bool {
	for row := 0; row < len(grid); row++ {
		l := 0
		r := len(grid[row]) - 1
		for l < r {
			grid[row][l], grid[row][r] = grid[row][r], grid[row][l]
			l++
			r--
		}
	}
	return grid
}

func toString(grid [][]bool) string {
	s := ""
	for _, row := range grid {
		for _, v := range row {
			if v {
				s += "#"
			} else {
				s += "."
			}
		}
	}
	return s
}

func fs2(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
		line := scanner.Text()
		_ = line
	}

	return 42
}

package main

import (
	"bufio"
	"fmt"
	"io"
	"math"
	"strings"

	lib "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	var grid [][]bool
	nRow := -1
	asteroids := make(map[lib.Position]bool)
	for scanner.Scan() {
		nRow++
		line := strings.TrimSpace(scanner.Text())
		var row []bool
		for col := 0; col < len(line); col++ {
			v := line[col]
			if v == '#' {
				row = append(row, true)
				asteroids[lib.Position{nRow, col}] = true
			} else {
				row = append(row, false)
			}
		}
		grid = append(grid, row)
	}

	max := 0
	for position := range asteroids {
		v := countVisibleAsteroinds(grid, position, asteroids)
		if v > max {
			max = v
		}
	}
	return max
}

func countVisibleAsteroinds(grid [][]bool, from lib.Position, asteroids map[lib.Position]bool) int {
	sum := 0
	for asteroid := range asteroids {
		if canDetect(grid, from, asteroid) {
			sum++
		}
	}
	return sum
}

func canDetect(grid [][]bool, from, to lib.Position) bool {
	if from == to {
		return false
	}

	distanceRow := to.Row - from.Row
	distanceCol := to.Col - from.Col

	distance := from.Manhattan(to)
	deltaRow := float64(distanceRow) / float64(distance)
	deltaCol := float64(distanceCol) / float64(distance)
	row := float64(from.Row) + deltaRow
	col := float64(from.Col) + deltaCol

	for i := 0; i < distance; i++ {
		if isInt(row) && isInt(col) {
			if int(math.Round(row)) == to.Row && int(math.Round(col)) == to.Col {
				return true
			}
			if grid[int(math.Round(row))][int(math.Round(col))] {
				return false
			}
		}

		row += deltaRow
		col += deltaCol
	}
	panic(fmt.Sprintf("%v, %v", from, to))
}

func isInt(f float64) bool {
	v := f - float64(int(f))
	return v < 0.000001 || v > 0.99999
}

func isZero(f float64) bool {
	v := f - float64(int(f))
	return v < 0.000001 || v > -0.000001
}

func fs2(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
		line := scanner.Text()
		_ = line
	}

	return 42
}

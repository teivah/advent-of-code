package main

import (
	"io"
	"sort"

	aoc "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	lines := aoc.ReaderToStrings(input)
	grid := make(map[aoc.Position]int)
	for row, line := range lines {
		for col := 0; col < len(line); col++ {
			grid[aoc.Position{row, col}] = aoc.RuneToInt(rune(line[col]))
		}
	}

	sum := 0
	for pos, h := range grid {
		n := getNeighbors(grid, pos)
		if allAbove(h, n) {
			sum += 1 + h
		}
	}

	return sum
}

func allAbove(h int, neighbors []int) bool {
	for _, neighbor := range neighbors {
		if neighbor <= h {
			return false
		}
	}
	return true
}

func getNeighbors(grid map[aoc.Position]int, pos aoc.Position) []int {
	var res []int

	if v, exists := grid[pos.Delta(-1, 0)]; exists {
		res = append(res, v)
	}
	if v, exists := grid[pos.Delta(1, 0)]; exists {
		res = append(res, v)
	}
	if v, exists := grid[pos.Delta(0, -1)]; exists {
		res = append(res, v)
	}
	if v, exists := grid[pos.Delta(0, 1)]; exists {
		res = append(res, v)
	}

	return res
}

func getNeighborPositions(grid map[aoc.Position]int, pos aoc.Position) []aoc.Position {
	var res []aoc.Position

	p := pos.Delta(-1, 0)
	if _, exists := grid[p]; exists {
		res = append(res, p)
	}
	p = pos.Delta(1, 0)
	if _, exists := grid[p]; exists {
		res = append(res, p)
	}
	p = pos.Delta(0, -1)
	if _, exists := grid[p]; exists {
		res = append(res, p)
	}
	p = pos.Delta(0, 1)
	if _, exists := grid[p]; exists {
		res = append(res, p)
	}

	return res
}

func fs2(input io.Reader) int {
	lines := aoc.ReaderToStrings(input)
	grid := make(map[aoc.Position]int)
	for row, line := range lines {
		for col := 0; col < len(line); col++ {
			grid[aoc.Position{row, col}] = aoc.RuneToInt(rune(line[col]))
		}
	}

	lowPoints := findLowPoints(grid)

	var res []int
	for _, lowPoint := range lowPoints {
		v := findLargest(grid, lowPoint, make(map[aoc.Position]bool))
		res = append(res, v)
	}

	sort.Ints(res)

	return res[len(res)-1] * res[len(res)-2] * res[len(res)-3]
}

func findLargest(grid map[aoc.Position]int, pos aoc.Position, visited map[aoc.Position]bool) int {
	if visited[pos] || grid[pos] == 9 {
		return 0
	}

	visited[pos] = true

	neighbors := getNeighborPositions(grid, pos)
	sum := 0
	for _, neighbor := range neighbors {
		if grid[neighbor] > grid[pos] {
			v := findLargest(grid, neighbor, visited)
			sum += v
		}
	}

	return 1 + sum
}

func findLowPoints(grid map[aoc.Position]int) []aoc.Position {
	var res []aoc.Position
	for pos, h := range grid {
		n := getNeighbors(grid, pos)
		if allAbove(h, n) {
			res = append(res, pos)
		}
	}
	return res
}

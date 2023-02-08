package main

import (
	"io"

	aoc "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader, steps int) int {
	lines := aoc.ReaderToStrings(input)
	grid := make(map[aoc.Position]int)
	for row, line := range lines {
		for col := 0; col < len(line); col++ {
			r := rune(line[col])
			grid[aoc.Position{row, col}] = aoc.RuneToInt(r)
		}
	}

	sum := 0
	for i := 0; i < steps; i++ {
		sum += round(grid)
	}

	return sum
}

func isSynchronized(grid map[aoc.Position]int) bool {
	for _, energy := range grid {
		if energy != 0 {
			return false
		}
	}
	return true
}

func round(grid map[aoc.Position]int) int {
	for pos := range grid {
		grid[pos]++
	}

	flashed := make(map[aoc.Position]bool)
	var q []aoc.Position
	for pos, energy := range grid {
		if energy > 9 {
			q = append(q, pos)
		}
	}

	for len(q) != 0 {
		pos := q[0]
		q = q[1:]
		q = append(q, flashes(grid, pos, flashed)...)
		flashed[pos] = true
	}

	sum := 0
	for pos, energy := range grid {
		if energy > 9 {
			sum++
			grid[pos] = 0
		}
	}
	return sum
}

func flashes(grid map[aoc.Position]int, pos aoc.Position, flashed map[aoc.Position]bool) []aoc.Position {
	if flashed[pos] {
		return nil
	}

	if _, exists := grid[pos]; !exists {
		return nil
	}

	var res []aoc.Position

	p := pos.Move(aoc.Up, 1)
	if flash(grid, p, flashed) {
		res = append(res, p)
	}

	p = pos.Move(aoc.Down, 1)
	if flash(grid, p, flashed) {
		res = append(res, p)
	}

	p = pos.Move(aoc.Left, 1)
	if flash(grid, p, flashed) {
		res = append(res, p)
	}

	p = pos.Move(aoc.Right, 1)
	if flash(grid, p, flashed) {
		res = append(res, p)
	}

	p = pos.Move(aoc.UpLeft, 1)
	if flash(grid, p, flashed) {
		res = append(res, p)
	}

	p = pos.Move(aoc.UpRight, 1)
	if flash(grid, p, flashed) {
		res = append(res, p)
	}

	p = pos.Move(aoc.DownLeft, 1)
	if flash(grid, p, flashed) {
		res = append(res, p)
	}

	p = pos.Move(aoc.DownRight, 1)
	if flash(grid, p, flashed) {
		res = append(res, p)
	}

	return res
}

func flash(grid map[aoc.Position]int, pos aoc.Position, flashed map[aoc.Position]bool) bool {
	if flashed[pos] {
		return false
	}

	if _, exists := grid[pos]; !exists {
		return false
	}

	grid[pos]++

	return grid[pos] == 10
}

func fs2(input io.Reader) int {
	lines := aoc.ReaderToStrings(input)
	grid := make(map[aoc.Position]int)
	for row, line := range lines {
		for col := 0; col < len(line); col++ {
			r := rune(line[col])
			grid[aoc.Position{row, col}] = aoc.RuneToInt(r)
		}
	}

	for i := 0; ; i++ {
		round(grid)
		if isSynchronized(grid) {
			return i + 1
		}
	}
}

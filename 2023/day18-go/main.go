package main

import (
	"bufio"
	"io"
	"strconv"

	aoc "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	edges, positions := parse(input, parseFromInstructions)
	return calculateArea(edges, positions)
}

func fs2(input io.Reader) int {
	edges, positions := parse(input, parseFromColor)
	return calculateArea(edges, positions)
}

func parse(input io.Reader, parseFunc func(string) (aoc.Direction, int)) (int, []aoc.Position) {
	var positions []aoc.Position
	pos := aoc.Position{}
	edges := 0

	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
		line := scanner.Text()
		dir, count := parseFunc(line)

		pos = pos.Move(dir, count)
		edges += count
		positions = append(positions, pos)
	}

	return edges, positions
}

func parseFromInstructions(line string) (aoc.Direction, int) {
	del := aoc.NewDelimiter(line, " ")
	direction := del.GetString(0)
	count := del.GetInt(1)

	var dir aoc.Direction
	switch direction {
	case "R":
		dir = aoc.Right
	case "D":
		dir = aoc.Down
	case "L":
		dir = aoc.Left
	case "U":
		dir = aoc.Up
	default:
		panic(direction)
	}

	return dir, count
}

func parseFromColor(line string) (aoc.Direction, int) {
	del := aoc.NewDelimiter(line, " ")
	s := del.GetString(2)
	color := s[2 : len(s)-1]

	distance := color[:5]
	count, err := strconv.ParseInt(distance, 16, 64)
	if err != nil {
		panic(err)
	}

	var dir aoc.Direction
	switch color[5] {
	case '0':
		dir = aoc.Right
	case '1':
		dir = aoc.Down
	case '2':
		dir = aoc.Left
	case '3':
		dir = aoc.Up
	default:
		panic(color)
	}

	return dir, int(count)
}

func calculateArea(edges int, positions []aoc.Position) int {
	// Source: https://stackoverflow.com/a/717367 (EDIT section)
	n := len(positions)
	positions = append(positions, aoc.Position{Row: positions[0].Row, Col: positions[0].Col})
	positions = append(positions, aoc.Position{Row: positions[1].Row, Col: positions[1].Col})
	area := 0
	for i := 1; i <= n; i++ {
		area += positions[i].Col * (positions[i+1].Row - positions[i-1].Row)
	}

	return area/2 + edges/2 + 1
}

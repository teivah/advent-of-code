package main

import (
	"bufio"
	"io"

	aoc "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	pos := aoc.Position{}
	for scanner.Scan() {
		line := scanner.Text()
		del := aoc.NewDelimiter(line, " ")
		n := del.GetInt(1)
		switch del.GetString(0) {
		case "forward":
			pos = pos.Move(aoc.Right, n)
		case "down":
			pos = pos.Move(aoc.Down, n)
		case "up":
			pos = pos.Move(aoc.Up, n)
		}
	}

	return pos.Row * pos.Col
}

func fs2(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	pos := aoc.Position{}
	aim := 0
	for scanner.Scan() {
		line := scanner.Text()
		del := aoc.NewDelimiter(line, " ")
		n := del.GetInt(1)
		switch del.GetString(0) {
		case "forward":
			pos = pos.Move(aoc.Right, n)
			pos = pos.Move(aoc.Down, aim*n)
		case "down":
			aim += n
		case "up":
			aim -= n
		}
	}

	return pos.Row * pos.Col
}

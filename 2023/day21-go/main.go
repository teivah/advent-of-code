package main

import (
	"bufio"
	"io"

	aoc "github.com/teivah/advent-of-code"
)

type squareType int

const (
	empty squareType = iota
	rock
)

func fs1(input io.Reader, iterations int) int {
	var start aoc.Position
	aoc.ParseBoard(aoc.ReaderToStrings(input), func(r rune) squareType {
		switch r {
		case '.':
			return empty
		case '#':
			return rock
		case 'S':
			start =
			return empty
		default:
			panic(r)
		}
	})

	return 42
}

func fs2(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
		line := scanner.Text()
		_ = line
	}

	return 42
}

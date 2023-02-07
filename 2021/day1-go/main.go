package main

import (
	"io"

	aoc "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	numbers := aoc.StringsToInts(aoc.ReaderToStrings(input))
	previous := numbers[0]
	sum := 0
	for i := 1; i < len(numbers); i++ {
		v := numbers[i]
		if v > previous {
			sum++
		}
		previous = v
	}

	return sum
}

func fs2(input io.Reader) int {
	numbers := aoc.StringsToInts(aoc.ReaderToStrings(input))
	previous := numbers[0] + numbers[1] + numbers[2]
	sum := 0
	for i := 1; i < len(numbers)-2; i++ {
		v := numbers[i] + numbers[i+1] + numbers[i+2]
		if v > previous {
			sum++
		}
		previous = v
	}

	return sum
}

package main

import (
	"bufio"
	"io"
	"math"

	aoc "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	groups := aoc.StringGroups(aoc.ReaderToStrings(input))

	seeds := parseSeeds(groups[0][0])
	var maps []map[int]int
	for i := 1; i < len(groups); i++ {
		maps = append(maps, parseMap(groups[i]))
	}

	lowest := math.MaxInt
	for _, seed := range seeds {
		for _, m := range maps {
			if v, exists := m[seed]; exists {
				seed = v
			}
		}
		lowest = min(lowest, seed)
	}

	return lowest
}

func parseSeeds(line string) []int {
	line = aoc.Substring(line, ": ")
	del := aoc.NewDelimiter(line, " ")
	return del.GetInts()
}

func parseMap(lines []string) map[int]int {
	res := make(map[int]int)
	for i := 0; i < len(lines); i++ {
		if i == 0 {
			// Discard header
			continue
		}
		del := aoc.NewDelimiter(lines[i], " ")
		ints := del.GetInts()
		dstRange := ints[0]
		srcRange := ints[1]
		rangeLength := ints[2]
		for j := 0; j < rangeLength; j++ {
			res[srcRange+j] = dstRange + j
		}
	}
	return res
}

func fs2(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
		line := scanner.Text()
		_ = line
	}

	return 42
}

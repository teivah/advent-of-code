package main

import (
	"io"
	"math"

	aoc "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	groups := aoc.StringGroups(aoc.ReaderToStrings(input))

	seeds := parseSeeds(groups[0][0])
	var maps []Map
	for i := 1; i < len(groups); i++ {
		maps = append(maps, parseMap(groups[i]))
	}

	lowest := math.MaxInt
	for _, v := range seeds {
		for _, m := range maps {
			if dst, contains := m.get(v); contains {
				v = dst
			}
		}
		lowest = min(lowest, v)
	}

	return lowest
}

func parseSeeds(line string) []int {
	line = aoc.Substring(line, ": ")
	del := aoc.NewDelimiter(line, " ")
	return del.GetInts()
}

type Range struct {
	// Included
	from int
	// Included
	to      int
	transfo int
}

type Map struct {
	ranges []Range
}

func (m Map) get(v int) (int, bool) {
	for _, r := range m.ranges {
		if v >= r.from && v <= r.to {
			return v + r.transfo, true
		}
	}
	return 0, false
}

func parseMap(lines []string) Map {
	var ranges []Range
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

		ranges = append(ranges, Range{
			from:    srcRange,
			to:      srcRange + rangeLength - 1,
			transfo: dstRange - srcRange,
		})
	}
	return Map{ranges: ranges}
}

func fs2(input io.Reader) int {
	groups := aoc.StringGroups(aoc.ReaderToStrings(input))

	seeds := parseSeedsRange(groups[0][0])
	var maps []Map
	for i := 1; i < len(groups); i++ {
		maps = append(maps, parseMap(groups[i]))
	}

	lowest := math.MaxInt
	for _, v := range seeds {
		for i := 0; i < v[1]; i++ {
			n := transform(v[0]+i, maps)
			lowest = min(lowest, n)
		}
	}

	return lowest
}

func transform(v int, maps []Map) int {
	for _, m := range maps {
		if dst, contains := m.get(v); contains {
			v = dst
		}
	}
	return v
}

func parseSeedsRange(line string) [][2]int {
	line = aoc.Substring(line, ": ")
	del := aoc.NewDelimiter(line, " ")
	ints := del.GetInts()
	var res [][2]int
	for i := 0; i < len(ints); i += 2 {
		res = append(res, [2]int{ints[i], ints[i+1]})
	}
	return res
}

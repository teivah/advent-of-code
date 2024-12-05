package main

import (
	"io"

	"github.com/teivah/go-aoc"
)

func fs1(input io.Reader) int {
	groups := aoc.StringGroups(aoc.ReaderToStrings(input))
	before, after := dep(groups[0])
	res := 0
	for _, line := range groups[1] {
		res += correctOrder(before, after, line)
	}
	return res
}

func dep(lines []string) (map[int]map[int]bool, map[int]map[int]bool) {
	before := make(map[int]map[int]bool)
	after := make(map[int]map[int]bool)
	for _, line := range lines {
		del := aoc.NewDelimiter(line, "|")
		src := del.GetInt(0)
		dst := del.GetInt(1)

		aoc.InnerMapGet(before, src)[dst] = true
		aoc.InnerMapGet(after, dst)[src] = true
	}
	return before, after
}

func correctOrder(before, after map[int]map[int]bool, line string) int {
	del := aoc.NewDelimiter(line, ",")
	ints := del.GetInts()
	for i := 0; i < len(ints); i++ {
		for j := i + 1; j < len(ints); j++ {
			x := ints[i]
			y := ints[j]

			if m, ok := before[y]; ok && m[x] {
				return 0
			}
			if m, ok := after[x]; ok && m[y] {
				return 0
			}
		}
	}

	return ints[len(ints)/2]
}

func fs2(input io.Reader) int {
	groups := aoc.StringGroups(aoc.ReaderToStrings(input))
	before, after := dep(groups[0])
	res := 0
	for _, line := range groups[1] {
		res += incorrectOrder(before, after, line)
	}
	return res
}

func incorrectOrder(before, after map[int]map[int]bool, line string) int {
	del := aoc.NewDelimiter(line, ",")
	ints := del.GetInts()
	invalid := false
outer:
	for i := 0; i < len(ints); i++ {
		for j := i + 1; j < len(ints); j++ {
			x := ints[i]
			y := ints[j]

			if m, ok := before[y]; ok && m[x] {
				invalid = true
				break outer
			}
			if m, ok := after[x]; ok && m[y] {
				invalid = true
				break outer
			}
		}
	}
	if !invalid {
		return 0
	}

	for _, v := range ints {
		countBefore := 0
		countAfter := 0
		for _, x := range ints {
			if x == v {
				continue
			}
			if before[v][x] {
				countBefore++
			} else if after[v][x] {
				countAfter++
			}
		}
		if countBefore == countAfter {
			return v
		}
	}

	panic("not valid")
}

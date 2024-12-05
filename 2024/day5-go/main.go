package main

import (
	"io"

	aoc "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	groups := aoc.StringGroups(aoc.ReaderToStrings(input))
	before, after := dep(groups[0])
	res := 0
	for _, line := range groups[1] {
		res += isRightOrder(before, after, line)
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

		m, ok := before[src]
		if !ok {
			m = make(map[int]bool)
			before[src] = m
		}
		m[dst] = true

		m, ok = after[dst]
		if !ok {
			m = make(map[int]bool)
			after[dst] = m
		}
		m[src] = true
	}
	return before, after
}

func isRightOrder(before, after map[int]map[int]bool, line string) int {
	del := aoc.NewDelimiter(line, ",")
	ints := del.GetInts()
	for i := 0; i < len(ints); i++ {
		for j := i + 1; j < len(ints); j++ {
			x := ints[i]
			y := ints[j]

			if m, ok := before[y]; ok {
				if m[x] {
					return 0
				}
			}
			if m, ok := after[x]; ok {
				if m[y] {
					return 0
				}
			}
		}
	}

	return ints[len(ints)/2]
}

func isIncorrectOrder(before, after map[int]map[int]bool, line string) int {
	del := aoc.NewDelimiter(line, ",")
	ints := del.GetInts()
	invalid := false
outer:
	for i := 0; i < len(ints); i++ {
		for j := i + 1; j < len(ints); j++ {
			x := ints[i]
			y := ints[j]

			if m, ok := before[y]; ok {
				if m[x] {
					invalid = true
					break outer
				}
			}
			if m, ok := after[x]; ok {
				if m[y] {
					invalid = true
					break outer
				}
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

func fs2(input io.Reader) int {
	groups := aoc.StringGroups(aoc.ReaderToStrings(input))
	before, after := dep(groups[0])
	res := 0
	for _, line := range groups[1] {
		res += isIncorrectOrder(before, after, line)
	}
	return res
}

package main

import (
	"io"

	lib "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	lines := lib.ReaderToStrings(input)

	groups := toGroup(lines)

	sum := 0
	for _, group := range groups {
		sum += count(group)
	}

	return sum
}

func count(group []string) int {
	set := make(map[rune]struct{})

	for _, s := range group {
		for i := 0; i < len(s); i++ {
			r := rune(s[i])
			set[r] = struct{}{}
		}
	}

	return len(set)
}

func toGroup(lines []string) [][]string {
	i := 0
	var res [][]string
	var row []string
	for {
		row = append(row, lines[i])
		i++
		if i >= len(lines) {
			res = append(res, row)
			break
		}
		for ; i < len(lines); i++ {
			if lines[i] == "" {
				break
			} else {
				row = append(row, lines[i])
			}
		}
		res = append(res, row)
		row = nil
		i++
		if i >= len(lines) {
			break
		}
	}
	return res
}

func fs2(input io.Reader) int {
	lines := lib.ReaderToStrings(input)

	groups := toGroup(lines)

	sum := 0
	for _, group := range groups {
		sum += countAll(group)
	}

	return sum
}

func countAll(group []string) int {
	people := len(group)
	m := make(map[rune]int)

	for _, s := range group {
		for i := 0; i < len(s); i++ {
			r := rune(s[i])
			m[r]++
		}
	}

	sum := 0
	for _, v := range m {
		if v == people {
			sum++
		}
	}

	return sum
}

package main

import (
	"io"
	"strings"

	aoc "github.com/teivah/advent-of-code"
)

const rock = '#'

func fs(input io.Reader, maxSmudge int) int {
	res := 0
	for _, group := range aoc.StringGroups(aoc.ReaderToStrings(input)) {
		if v, found := findHorizontal(group, maxSmudge); found {
			res += 100 * v
			continue
		}
		if v, found := findVertical(group, maxSmudge); found {
			res += v
			continue
		}
		panic(group)
	}

	return res
}

func findHorizontal(lines []string, maxSmudge int) (int, bool) {
	rows := make([]int, len(lines))
	variations := make([][]int, len(lines))
	for row, line := range lines {
		rows[row], variations[row] = toHashAndVariations(line)
	}

	return find(rows, variations, len(lines[0]), maxSmudge)
}

func findVertical(lines []string, maxSmudge int) (int, bool) {
	cols := make([]int, len(lines[0]))
	variations := make([][]int, len(lines[0]))
	for col := range lines[0] {
		cols[col], variations[col] = toHashAndVariations(getColString(lines, col))
	}

	return find(cols, variations, len(lines), maxSmudge)
}

func getColString(lines []string, col int) string {
	sb := strings.Builder{}
	sb.Grow(len(lines))
	for row := 0; row < len(lines); row++ {
		sb.WriteRune(rune(lines[row][col]))
	}
	return sb.String()
}

func toHashAndVariations(s string) (int, []int) {
	hash := 0
	for i := 0; i < len(s); i++ {
		if s[i] == rock {
			hash += 1 << i
		}
	}

	variations := make([]int, len(s))
	for i := 0; i < len(s); i++ {
		v := 0
		if s[i] == rock {
			v = hash & ^(1 << i)
		} else {
			v = hash + 1<<i
		}
		variations[i] = v
	}

	return hash, variations
}

func find(hashes []int, variations [][]int, max int, maxSmudge int) (int, bool) {
nextLine:
	for i := 1; i < len(hashes); i++ {
		smudges := 0

	nextHash:
		for idx1, idx2 := i-1, i; idx1 >= 0 && idx2 < len(hashes); idx1, idx2 = idx1-1, idx2+1 {
			if hashes[idx1] == hashes[idx2] {
				continue
			}
			if smudges == maxSmudge {
				continue nextLine
			}

			// If the hashes do not match, we check the variations.
			for j := 0; j < max; j++ {
				if hashes[idx1] == variations[idx2][j] || variations[idx1][j] == hashes[idx2] {
					smudges++
					continue nextHash
				}
			}
			continue nextLine
		}
		if smudges == maxSmudge {
			return i, true
		}
	}

	return 0, false
}

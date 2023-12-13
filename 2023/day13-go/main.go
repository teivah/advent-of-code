package main

import (
	"io"
	"strings"

	aoc "github.com/teivah/advent-of-code"
)

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
	for i, line := range lines {
		h, v := toHashAndVariations(line)
		rows[i] = h
		variations[i] = v
	}

	return find(rows, variations, len(lines[0]), maxSmudge)
}

func findVertical(lines []string, maxSmudge int) (int, bool) {
	cols := make([]int, len(lines[0]))
	variations := make([][]int, len(lines[0]))
	for i := 0; i < len(lines[0]); i++ {
		sb := strings.Builder{}
		sb.Grow(len(lines[0]))
		for row := 0; row < len(lines); row++ {
			sb.WriteRune(rune(lines[row][i]))
		}
		h, v := toHashAndVariations(sb.String())
		cols[i] = h
		variations[i] = v
	}

	return find(cols, variations, len(lines), maxSmudge)
}

func toHashAndVariations(s string) (int, []int) {
	hash := 0
	for i := 0; i < len(s); i++ {
		if s[i] == '#' {
			hash += 1 << i
		}
	}

	var variations []int
	for i := 0; i < len(s); i++ {
		if s[i] == '#' {
			v := hash & ^(1 << i)
			variations = append(variations, v)
		} else {
			v := hash + 1<<i
			variations = append(variations, v)
		}
	}

	return hash, variations
}

func find(hashes []int, variations [][]int, max int, maxSmudge int) (int, bool) {
outer:
	for i := 1; i < len(hashes); i++ {
		smudges := 0
		for idx1, idx2 := i-1, i; ; idx1, idx2 = idx1-1, idx2+1 {
			if idx1 < 0 || idx2 >= len(hashes) {
				break
			}
			if hashes[idx1] == hashes[idx2] {
				continue
			}

			if smudges == maxSmudge {
				continue outer
			}

			// If the hashes do not match, we check the variations.
			found := false
			for j := 0; j < max; j++ {
				if hashes[idx1] == variations[idx2][j] || variations[idx1][j] == hashes[idx2] {
					smudges++
					found = true
					break
				}
			}
			if !found {
				continue outer
			}
		}
		if smudges == maxSmudge {
			return i, true
		}
	}

	return 0, false
}

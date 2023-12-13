package main

import (
	"io"
	"strings"

	aoc "github.com/teivah/advent-of-code"
)

func fs(input io.Reader, maxSmudge int) int {
	groups := aoc.StringGroups(aoc.ReaderToStrings(input))

	res := 0
	for _, group := range groups {
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
	hashes := make([]int, 0, len(lines))
	variations := make([][]int, 0, len(lines))
	for _, line := range lines {
		h, v := getHashAndVariations(line)
		hashes = append(hashes, h)
		variations = append(variations, v)
	}

	return find(hashes, variations, len(lines[0]), maxSmudge)
}

func findVertical(lines []string, maxSmudge int) (int, bool) {
	hashes := make([]int, 0, len(lines))
	variations := make([][]int, 0, len(lines))

	for col := 0; col < len(lines[0]); col++ {
		sb := strings.Builder{}
		sb.Grow(len(lines[0]))
		for row := 0; row < len(lines); row++ {
			sb.WriteRune(rune(lines[row][col]))
		}
		h, v := getHashAndVariations(sb.String())
		hashes = append(hashes, h)
		variations = append(variations, v)
	}

	return find(hashes, variations, len(lines), maxSmudge)
}

func getHashAndVariations(s string) (int, []int) {
	res := 0
	for i := 0; i < len(s); i++ {
		if s[i] == '#' {
			res += 1 << i
		}
	}

	var variations []int
	for i := 0; i < len(s); i++ {
		if s[i] == '#' {
			v := res & ^(1 << i)
			variations = append(variations, v)
		} else {
			v := res + 1<<i
			variations = append(variations, v)
		}
	}

	return res, variations
}

func find(hashes []int, variations [][]int, max int, maxSmudge int) (int, bool) {
outer:
	for i := 1; i < len(hashes); i++ {
		smudge := 0
		for idx1, idx2 := i-1, i; ; idx1, idx2 = idx1-1, idx2+1 {
			if idx1 < 0 || idx2 >= len(hashes) {
				break
			}
			if hashes[idx1] == hashes[idx2] {
				continue
			}

			if smudge == maxSmudge {
				continue outer
			}

			// If the hashes do not match, we check the variations.
			found := false
			for j := 0; j < max; j++ {
				if hashes[idx1] == variations[idx2][j] || variations[idx1][j] == hashes[idx2] {
					smudge++
					found = true
					break
				}
			}
			if !found {
				continue outer
			}
		}
		if smudge == maxSmudge {
			return i, true
		}
	}

	return 0, false
}

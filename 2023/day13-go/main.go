package main

import (
	"io"
	"strings"

	aoc "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	groups := aoc.StringGroups(aoc.ReaderToStrings(input))

	res := 0
	for _, group := range groups {
		if v, found := findHorizontal(group); found {
			res += 100 * v
			continue
		}
		if v, found := findVertical(group); found {
			res += v
			continue
		}
		panic(group)
	}

	return res
}

func findHorizontal(lines []string) (int, bool) {
	numbers := make([]int, 0, len(lines))
	for _, line := range lines {
		numbers = append(numbers, stringToNumber(line))
	}

	for i := 1; i < len(numbers); i++ {
		found := true
		for j := 0; j < i; j++ {
			idx1 := i - j - 1
			idx2 := i + j
			if idx1 < 0 || idx2 >= len(numbers) {
				break
			}
			if numbers[idx1] != numbers[idx2] {
				found = false
				break
			}
		}
		if found {
			return i, true
		}
	}

	return 0, false
}

func findVertical(lines []string) (int, bool) {
	numbers := make([]int, 0, len(lines))

	for col := 0; col < len(lines[0]); col++ {
		sb := strings.Builder{}
		sb.Grow(len(lines[0]))
		for row := 0; row < len(lines); row++ {
			sb.WriteRune(rune(lines[row][col]))
		}
		numbers = append(numbers, stringToNumber(sb.String()))
	}

	for i := 1; i < len(numbers); i++ {
		found := true
		for j := 0; j < i; j++ {
			idx1 := i - j - 1
			idx2 := i + j
			if idx1 < 0 || idx2 >= len(numbers) {
				break
			}
			if numbers[idx1] != numbers[idx2] {
				found = false
				break
			}
		}
		if found {
			return i, true
		}
	}

	return 0, false
}

func stringToNumber(s string) int {
	res := 0
	for i := 0; i < len(s); i++ {
		if s[i] == '#' {
			res += 1 << i
		}
	}
	return res
}

func fs2(input io.Reader) int {
	groups := aoc.StringGroups(aoc.ReaderToStrings(input))

	res := 0
	for _, group := range groups {
		if v, found := findHorizontal2(group); found {
			res += 100 * v
			continue
		}
		if v, found := findVertical2(group); found {
			res += v
			continue
		}
		panic(group)
	}

	return res
}

func stringToNumberAndVariations(s string) (int, []int) {
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

func findHorizontal2(lines []string) (int, bool) {
	numbers := make([]int, 0, len(lines))
	allVariations := make([][]int, 0, len(lines))
	for _, line := range lines {
		number, variations := stringToNumberAndVariations(line)
		numbers = append(numbers, number)
		allVariations = append(allVariations, variations)
	}

	for i := 1; i < len(numbers); i++ {
		smudge := 0
		for j := 0; j < i; j++ {
			idx1 := i - j - 1
			idx2 := i + j
			if idx1 < 0 || idx2 >= len(numbers) {
				break
			}
			if numbers[idx1] == numbers[idx2] {
				continue
			}

			if smudge == 1 {
				smudge = -1
				break
			}

			found := false
			for col := 0; col < len(lines[0]); col++ {
				if numbers[idx1] == allVariations[idx2][col] || allVariations[idx1][col] == numbers[idx2] {
					smudge++
					found = true
					break
				}
			}
			if !found {
				smudge = -1
				break
			}
		}
		if smudge == 1 {
			return i, true
		}
	}

	return 0, false
}

func findVertical2(lines []string) (int, bool) {
	numbers := make([]int, 0, len(lines))
	allVariations := make([][]int, 0, len(lines))

	for col := 0; col < len(lines[0]); col++ {
		sb := strings.Builder{}
		sb.Grow(len(lines[0]))
		for row := 0; row < len(lines); row++ {
			sb.WriteRune(rune(lines[row][col]))
		}
		number, variations := stringToNumberAndVariations(sb.String())
		numbers = append(numbers, number)
		allVariations = append(allVariations, variations)
	}

	for i := 1; i < len(numbers); i++ {
		smudge := 0
		for j := 0; j < i; j++ {
			idx1 := i - j - 1
			idx2 := i + j
			if idx1 < 0 || idx2 >= len(numbers) {
				break
			}
			if numbers[idx1] == numbers[idx2] {
				continue
			}

			if smudge == 1 {
				smudge = -1
				break
			}

			found := false
			for col := 0; col < len(lines); col++ {
				if numbers[idx1] == allVariations[idx2][col] || allVariations[idx1][col] == numbers[idx2] {
					smudge++
					found = true
					break
				}
			}
			if !found {
				smudge = -1
				break
			}
		}
		if smudge == 1 {
			return i, true
		}
	}

	return 0, false
}

package main

import (
	"io"
	"strings"

	"github.com/teivah/go-aoc"
)

func fs1(input io.Reader) int {
	patterns, designs := parse(input)
	dict := make(map[uint8][]string)
	for _, pattern := range patterns {
		dict[pattern[0]] = append(dict[pattern[0]], pattern)
	}
	res := 0
	cache := make(map[string]bool)
	for _, design := range designs {
		if solvable(design, dict, cache) {
			res++
		}
	}
	return res
}

func parse(input io.Reader) ([]string, []string) {
	groups := aoc.StringGroups(aoc.ReaderToStrings(input))
	del := aoc.NewDelimiter(groups[0][0], ", ")
	return del.GetStrings(), groups[1]
}

func solvable(s string, dict map[uint8][]string, cache map[string]bool) bool {
	if len(s) == 0 {
		return true
	}

	if found, ok := cache[s]; ok {
		return found
	}

	patterns, ok := dict[s[0]]
	if !ok {
		cache[s] = false
		return false
	}
	for _, pattern := range patterns {
		if len(pattern) > len(s) {
			continue
		}
		if !strings.HasPrefix(s, pattern) {
			continue
		}
		if solvable(s[len(pattern):], dict, cache) {
			cache[s] = true
			return true
		}
	}
	cache[s] = false
	return false
}

func fs2(input io.Reader) int {
	patterns, designs := parse(input)
	dict := make(map[uint8][]string)
	for _, pattern := range patterns {
		dict[pattern[0]] = append(dict[pattern[0]], pattern)
	}
	res := 0
	cache := make(map[string]int)
	for _, design := range designs {
		res += solvable2(design, dict, cache)
	}
	return res
}

func solvable2(s string, dict map[uint8][]string, cache map[string]int) int {
	if len(s) == 0 {
		return 1
	}

	if found, ok := cache[s]; ok {
		return found
	}

	patterns, ok := dict[s[0]]
	if !ok {
		cache[s] = 0
		return 0
	}
	count := 0
	for _, pattern := range patterns {
		if len(pattern) > len(s) {
			continue
		}
		if !strings.HasPrefix(s, pattern) {
			continue
		}
		count += solvable2(s[len(pattern):], dict, cache)
	}
	cache[s] = count
	return count
}

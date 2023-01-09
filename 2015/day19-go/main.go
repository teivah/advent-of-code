package main

import (
	"bufio"
	"io"
	"math"
	"strings"
)

func fs1(input io.Reader) (int, error) {
	scanner := bufio.NewScanner(input)
	m := make(map[string][]string)
	start := ""
	for scanner.Scan() {
		s := scanner.Text()
		if s == "" {
			scanner.Scan()
			start = scanner.Text()
			break
		}
		idx := indexAll(s, " ")
		m[s[:idx[0]]] = append(m[s[:idx[0]]], s[idx[1]+1:])
	}

	set := make(map[string]struct{})
	for i := 0; i < len(start); i++ {
		s := start[i : i+1]
		if options, exists := m[s]; exists {
			for _, v := range options {
				res := start[:i] + v + start[i+1:]
				set[res] = struct{}{}
			}
		}

		if i != len(start)-1 {
			s = start[i : i+2]
			if options, exists := m[s]; exists {
				for _, v := range options {
					res := start[:i] + v + start[i+2:]
					set[res] = struct{}{}
				}
			}
		}
	}

	return len(set), nil
}

func fs2(input io.Reader) (int, error) {
	scanner := bufio.NewScanner(input)
	m := make(map[string][]string)
	target := ""
	for scanner.Scan() {
		s := scanner.Text()
		if s == "" {
			scanner.Scan()
			target = scanner.Text()
			break
		}
		idx := indexAll(s, " ")
		m[s[:idx[0]]] = append(m[s[:idx[0]]], s[idx[1]+1:])
	}

	start := "e"

	cache = make(map[string]int)

	return best(start, target, m), nil
}

var cache map[string]int

func best(start, target string, replacements map[string][]string) int {
	if start == target {
		return 0
	}

	if len(start) >= len(target) {
		return math.MaxInt
	}

	if v, exists := cache[start]; exists {
		if v == math.MaxInt {
			return v
		}
		return v + 1
	}

	// To avoid cycles
	cache[start] = math.MaxInt

	minScore := math.MaxInt
	for i := 0; i < len(start); i++ {
		if i > 0 && start[i-1] != target[i-1] {
			break
		}

		if options, exists := replacements[start[i:i+1]]; exists {
			for _, v := range options {
				res := start[:i] + v + start[i+1:]
				score := best(res, target, replacements)
				if score == math.MaxInt {
					continue
				}
				minScore = min(minScore, score)
			}
		}

		if i != len(start)-1 {
			if options, exists := replacements[start[i:i+2]]; exists {
				for _, v := range options {
					res := start[:i] + v + start[i+2:]
					score := best(res, target, replacements)
					if score == math.MaxInt {
						continue
					}
					minScore = min(minScore, score)
				}
			}
		}
	}

	cache[start] = minScore

	if minScore == math.MaxInt {
		return math.MaxInt
	}

	return minScore + 1
}

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func indexAll(s string, search string) []int {
	i := 0
	var res []int
	for i < len(s) {
		index := strings.Index(s[i:], search)
		if index == -1 {
			return res
		}
		res = append(res, index+i)
		i += index + len(search)
	}
	return res
}

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
	reverse := make(map[string]string)
	target := ""
	for scanner.Scan() {
		s := scanner.Text()
		if s == "" {
			scanner.Scan()
			target = scanner.Text()
			break
		}
		idx := indexAll(s, " ")
		from := s[:idx[0]]
		to := s[idx[1]+1:]
		m[from] = append(m[from], to)
		reverse[to] = from
	}

	start := "e"

	return best(target, start, reverse), nil
}

func best(start, target string, reverse map[string]string) int {
	if start == target {
		return 0
	}

	for to, from := range reverse {
		all := indexAll(start, to)
		for _, idx := range all {
			s := start[:idx] + from + start[idx+len(to):]
			v := best(s, target, reverse)
			if v != math.MaxInt {
				return v + 1
			}
		}
	}

	return math.MaxInt
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

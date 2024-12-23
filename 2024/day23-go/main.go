package main

import (
	"io"
	"sort"
	"strings"

	"github.com/teivah/go-aoc"
)

func fs1(input io.Reader) int {
	lines := aoc.ReaderToStrings(input)
	graph := make(map[string][]string)
	linked := make(map[aoc.Pair[string, string]]bool)
	for _, line := range lines {
		del := aoc.NewDelimiter(line, "-")
		a, b := del.GetString(0), del.GetString(1)
		graph[a] = append(graph[a], b)
		graph[b] = append(graph[b], a)
		linked[aoc.Pair[string, string]{a, b}] = true
		linked[aoc.Pair[string, string]{b, a}] = true
	}
	sets := make(map[tri]bool)
	for from, tos := range graph {
		group(from, tos, sets, linked)
	}
	res := 0
	for s := range sets {
		if s.startsWithT() {
			res++
		}
	}
	return res
}

type tri struct {
	a string
	b string
	c string
}

func (t tri) startsWithT() bool {
	return t.a[0] == 't' ||
		t.b[0] == 't' ||
		t.c[0] == 't'
}

func newSet(machines ...string) tri {
	if len(machines) != 3 {
		panic(len(machines))
	}
	sort.Strings(machines)
	return tri{machines[0], machines[1], machines[2]}
}

func group(a string, tos []string, sets map[tri]bool, linked map[aoc.Pair[string, string]]bool) {
	for i := 0; i < len(tos); i++ {
		b := tos[i]
		for j := i + 1; j < len(tos); j++ {
			c := tos[j]
			if linked[aoc.Pair[string, string]{b, c}] {
				sets[newSet(a, b, c)] = true
			}
		}
	}
}

func fs2(input io.Reader) string {
	lines := aoc.ReaderToStrings(input)
	graph := make(map[string][]string)
	linked := make(map[aoc.Pair[string, string]]bool)
	for _, line := range lines {
		del := aoc.NewDelimiter(line, "-")
		a, b := del.GetString(0), del.GetString(1)
		graph[a] = append(graph[a], b)
		graph[b] = append(graph[b], a)
		linked[aoc.Pair[string, string]{a, b}] = true
		linked[aoc.Pair[string, string]{b, a}] = true
	}

	sets := make(map[string]bool)
	for from, tos := range graph {
		group2(from, tos, sets, linked)
	}
	res := ""
	for s := range sets {
		if areAllConnected(aoc.NewDelimiter(s, ",").GetStrings(), linked) {
			if len(s) > len(res) {
				res = s
			}
		}
	}
	return res
}

func areAllConnected(s []string, linked map[aoc.Pair[string, string]]bool) bool {
	for i := 0; i < len(s); i++ {
		for j := i + 1; j < len(s); j++ {
			if !linked[aoc.Pair[string, string]{s[i], s[j]}] {
				return false
			}
		}
	}
	return true
}

func group2(a string, tos []string, sets map[string]bool, linked map[aoc.Pair[string, string]]bool) {
	for i := 0; i < len(tos); i++ {
		b := tos[i]
		var connected []string
		for j := i + 1; j < len(tos); j++ {
			c := tos[j]
			if linked[aoc.Pair[string, string]{b, c}] {
				connected = append(connected, c)
			}
		}
		connected = append(connected, a, b)
		sort.Strings(connected)
		sets[strings.Join(connected, ",")] = true
	}
}

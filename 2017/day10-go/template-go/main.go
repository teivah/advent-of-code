package main

import (
	lib "github.com/teivah/advent-of-code"
	"io"
	"strings"
)

func fs1(input io.Reader) int {
	s := lib.ReaderToString(input)

	directions := strings.Split(s, ",")
	d := make(map[string]int)
	for _, direction := range directions {
		d[direction]++
	}

	// Horizontal
	remove(d, "n", "s")

	// Diagonal 1
	remove(d, "ne", "sw")

	// Diagonal 2
	remove(d, "nw", "se")

	clean(d, "ne", "s")
	clean(d, "ne", "nw")

	clean(d, "se", "n")
	clean(d, "se", "sw")

	clean(d, "sw", "n")
	clean(d, "sw", "se")

	clean(d, "nw", "s")
	clean(d, "nw", "ne")

	sum := 0
	for _, v := range d {
		sum += v
	}
	return sum
}

func remove(d map[string]int, a, b string) {
	min := lib.Min(d[a], d[b])
	d[a] -= min
	d[b] -= min
}

func clean(d map[string]int, a, b string) {
	min := lib.Min(d[a], d[b])
	if d[a] == min {
		d[a] = 0
	} else {
		d[b] = 0
	}
}

func fs2(input io.Reader) int {
	s := lib.ReaderToString(input)

	directions := strings.Split(s, ",")
	d := make(map[string]int)
	for _, direction := range directions {
		d[direction]++
	}

	// Horizontal
	remove(d, "n", "s")

	// Diagonal 1
	remove(d, "ne", "sw")

	// Diagonal 2
	remove(d, "nw", "se")

	clean(d, "ne", "s")
	clean(d, "ne", "nw")

	clean(d, "se", "n")
	clean(d, "se", "sw")

	clean(d, "sw", "n")
	clean(d, "sw", "se")

	clean(d, "nw", "s")
	clean(d, "nw", "ne")

	sum := 0
	for _, v := range d {
		sum += v
	}
	return sum
}

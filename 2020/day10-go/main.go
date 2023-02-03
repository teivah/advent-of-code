package main

import (
	"io"
	"sort"

	lib "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	jolts := lib.ReaderToInts(input)
	sort.Ints(jolts)

	jolts = append([]int{0}, jolts...)
	jolts = append(jolts, jolts[len(jolts)-1]+3)

	diffs := make(map[int]int)
	for i := 0; i < len(jolts)-1; i++ {
		diff := jolts[i+1] - jolts[i]
		diffs[diff]++
	}

	return diffs[1] * diffs[3]
}

func fs2(input io.Reader) int {
	jolts := lib.ReaderToInts(input)
	sort.Ints(jolts)

	jolts = append([]int{0}, jolts...)
	jolts = append(jolts, jolts[len(jolts)-1]+3)

	c := counter{
		visited: make(map[int]int),
	}
	return c.count(0, jolts)
}

type counter struct {
	visited map[int]int
}

func (c *counter) count(i int, jolts []int) int {
	if v, exists := c.visited[i]; exists {
		return v
	}

	if i == len(jolts)-1 {
		return 1
	}

	sum := 0
	for j := i + 1; ; j++ {
		if j >= len(jolts) {
			break
		}

		if jolts[j] > jolts[i]+3 {
			break
		}
		sum += c.count(j, jolts)
	}

	c.visited[i] = sum
	return sum
}

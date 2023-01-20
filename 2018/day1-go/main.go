package main

import (
	"bufio"
	"io"

	lib "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	sum := 0
	for scanner.Scan() {
		sum += lib.StringToInt(scanner.Text())
	}

	return sum
}

func fs2(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	sum := 0
	set := make(map[int]struct{})
	set[0] = struct{}{}
	var values []int
	for scanner.Scan() {
		values = append(values, lib.StringToInt(scanner.Text()))
	}

	for i := 0; ; i = (i + 1) % len(values) {
		sum += values[i]
		if _, exists := set[sum]; exists {
			return sum
		}
		set[sum] = struct{}{}
	}
}

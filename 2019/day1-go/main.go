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
		line := scanner.Text()
		mass := lib.StringToInt(line)
		sum += (mass / 3) - 2
	}

	return sum
}

func fs2(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	sum := 0
	for scanner.Scan() {
		module := lib.StringToInt(scanner.Text())
		fuel := getMass(module)
		sum += fuel
	}

	return sum
}

func getMass(m int) int {
	if m <= 0 {
		return 0
	}

	v := (m / 3) - 2
	if v <= 0 {
		return 0
	}
	return v + getMass(v)
}

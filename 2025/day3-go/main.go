package main

import (
	"io"

	"github.com/teivah/go-aoc"
)

func fs1(input io.Reader) int {
	res := 0
	for _, line := range aoc.ReaderToStrings(input) {
		runes := []rune(line)
		maxValue := -1
		best := -1
		for i := 0; i < len(runes)-1; i++ {
			value := aoc.RuneToInt(runes[i])
			if value > maxValue {
				best = i
				maxValue = value
			}
		}

		maxValue2 := -1
		for i := best + 1; i < len(runes); i++ {
			maxValue2 = max(maxValue2, aoc.RuneToInt(runes[i]))
		}
		res += maxValue*10 + maxValue2
	}
	return res
}

func fs2(input io.Reader) int {
	res := 0
	const digits = 12
	for _, line := range aoc.ReaderToStrings(input) {
		runes := []rune(line)
		found := make([]int, digits)
		previous := -1

		for level := 0; level < digits; level++ {
			maxValue := -1
			best := -1
			remaining := digits - level - 1
			for i := previous + 1; i < len(runes)-remaining; i++ {
				if i >= len(runes) {
					break
				}
				value := aoc.RuneToInt(runes[i])
				if value > maxValue {
					maxValue = value
					best = i
				}
			}
			found[digits-level-1] = maxValue
			previous = best
		}

		v := 0
		mult := 1
		for _, n := range found {
			v += n * mult
			mult *= 10
		}
		res += v
	}
	return res
}

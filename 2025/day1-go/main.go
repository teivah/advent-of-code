package main

import (
	"io"

	"github.com/teivah/go-aoc"
)

func fs1(input io.Reader) int {
	lines := aoc.ReaderToStrings(input)
	cur := 50
	res := 0
	for _, line := range lines {
		left := line[0] == 'L'
		count := aoc.StringToInt(line[1:])
		if left {
			cur = aoc.Mod(cur-count, 100)
		} else {
			cur = aoc.Mod(cur+count, 100)
		}
		if cur == 0 {
			res++
		}
	}
	return res
}

func fs2(input io.Reader) int {
	lines := aoc.ReaderToStrings(input)
	cur := 50
	res := 0
	for _, line := range lines {
		left := line[0] == 'L'
		count := aoc.StringToInt(line[1:])
		if left {
			for range count {
				cur--
				if cur == -1 {
					cur = 99
				}
				if cur == 0 {
					res++
				}
			}
		} else {
			for range count {
				cur++
				if cur == 100 {
					cur = 0
				}
				if cur == 0 {
					res++
				}
			}
		}
	}
	return res
}

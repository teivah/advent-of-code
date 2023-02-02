package main

import (
	"io"
	"sort"

	lib "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	s := lib.ReaderToInts(input)
	sort.Ints(s)

	const target = 2020
	l := 0
	r := len(s) - 1
	for l < r {
		a := s[l]
		b := s[r]
		sum := a + b
		if sum == target {
			return a * b
		}

		if sum < target {
			l++
		} else {
			r--
		}
	}

	return -1
}

func fs2(input io.Reader) int {
	s := lib.ReaderToInts(input)
	sort.Ints(s)

	const target = 2020

	for i := 0; i < len(s)-2; i++ {
		l := i + 1
		r := len(s) - 1
		for l < r {
			x := s[i]
			a := s[l]
			b := s[r]
			sum := a + b + x
			if sum == target {
				return a * b * x
			}

			if sum < target {
				l++
			} else {
				r--
			}
		}

	}

	return -1
}

package main

import (
	"io"
	"strings"

	lib "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader, m map[int]int) int {
	s := lib.ReaderToString(input)
	codes := lib.StringsToInts(strings.Split(s, ","))

	for k, v := range m {
		codes[k] = v
	}

	for i := 0; i < len(codes); i += 4 {
		if codes[i] == 1 {
			codes[codes[i+3]] = codes[codes[i+1]] + codes[codes[i+2]]
		} else if codes[i] == 2 {
			codes[codes[i+3]] = codes[codes[i+1]] * codes[codes[i+2]]
		} else if codes[i] == 99 {
			break
		}
	}

	return codes[0]
}

func fs2(input io.Reader) int {
	s := lib.ReaderToString(input)
	codes := lib.StringsToInts(strings.Split(s, ","))

	for noun := 0; noun < len(codes); noun++ {
		for verb := 0; verb < len(codes); verb++ {
			codes[1] = noun
			codes[2] = verb

			c := make([]int, len(codes))
			copy(c, codes)

			for i := 0; i < len(c); i += 4 {
				if c[i] == 1 {
					c[c[i+3]] = c[c[i+1]] + c[c[i+2]]
				} else if c[i] == 2 {
					c[c[i+3]] = c[c[i+1]] * c[c[i+2]]
				} else if c[i] == 99 {
					break
				}
			}

			if c[0] == 19690720 {
				return 100*noun + verb
			}
		}
	}

	return -1
}

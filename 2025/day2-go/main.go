package main

import (
	"io"
	"strconv"

	"github.com/teivah/go-aoc"
)

func fs1(input io.Reader) int {
	line := aoc.ReaderToString(input)
	del := aoc.NewDelimiter(line, ",")
	res := 0
	for _, ranges := range del.GetStrings() {
		del := aoc.NewDelimiter(ranges, "-")
		from := del.GetString(0)
		to := del.GetString(1)
		for i := aoc.StringToInt(from); i <= aoc.StringToInt(to); i++ {
			if isInvalid(strconv.Itoa(i)) {
				res += i
			}
		}
	}
	return res
}

func isInvalid(id string) bool {
	if len(id)%2 != 0 {
		return false
	}
	return id[:len(id)/2] == id[len(id)/2:]
}

func fs2(input io.Reader) int {
	line := aoc.ReaderToString(input)
	del := aoc.NewDelimiter(line, ",")
	res := 0
	for _, ranges := range del.GetStrings() {
		del := aoc.NewDelimiter(ranges, "-")
		from := del.GetString(0)
		to := del.GetString(1)
		for i := aoc.StringToInt(from); i <= aoc.StringToInt(to); i++ {
			if isInvalid2(strconv.Itoa(i)) {
				res += i
			}
		}
	}
	return res
}

func isInvalid2(id string) bool {
	for i := 1; i <= len(id)/2; i++ {
		if len(id)%i != 0 {
			continue
		}

		pattern := id[:i]
		found := true
		for j := i; j < len(id); j += i {
			cur := id[j : j+i]
			if pattern == cur {
				continue
			} else {
				found = false
				break
			}
		}
		if found {
			return true
		}
	}
	return false
}

package aoc

import (
	"fmt"
	"strings"
)

// FindStringIndices returns all the indices from a given string into a string.
func FindStringIndices(s string, search string) []int {
	i := 0
	var res []int
	for i < len(s) {
		index := strings.Index(s[i:], search)
		if index == -1 {
			return res
		}
		res = append(res, index+i)
		i += index + len(search)
	}
	return res
}

// Substring returns the substring from a given delimiter.
func Substring(s string, del string) string {
	idx := strings.Index(s, del)
	if idx == -1 {
		panic(fmt.Sprintf("substring: %s is not present in %s", del, s))
	}
	return s[idx+len(del):]
}

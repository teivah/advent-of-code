package main

import (
	"bufio"
	"io"
	"strings"

	lib "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	two := 0
	three := 0
	for scanner.Scan() {
		s := scanner.Text()
		if letters(s, 2) {
			two++
		}
		if letters(s, 3) {
			three++
		}
	}

	return two * three
}

func letters(s string, count int) bool {
	set := make(map[rune]int)
	for i := 0; i < len(s); i++ {
		r := rune(s[i])
		set[r]++
	}

	for _, v := range set {
		if v == count {
			return true
		}
	}

	return false
}

func fs2(input io.Reader) string {
	words := lib.ReaderToStrings(input)

	set := make(map[string]int)
	for _, s := range words {
		for empty := 0; empty < len(s); empty++ {
			sb := strings.Builder{}
			for i := 0; i < empty; i++ {
				sb.WriteByte(s[i])
			}
			sb.WriteRune('_')
			for i := empty + 1; i < len(s); i++ {
				sb.WriteByte(s[i])
			}
			set[sb.String()]++
		}
	}

	for k, v := range set {
		if v == 2 {
			sb := strings.Builder{}
			for i := 0; i < len(k); i++ {
				r := rune(k[i])
				if r == '_' {
					continue
				}
				sb.WriteRune(r)
			}
			return sb.String()
		}
	}

	return ""
}

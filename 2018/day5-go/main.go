package main

import (
	"io"
	"strings"
	"unicode"

	lib "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	s := lib.ReaderToString(input)
	return reactions(s)
}

func reactions(s string) int {
	for {
		s2 := reaction(s)
		if s2 == s {
			return len(s)
		}
		s = s2
	}
}

func reaction(s string) string {
	sb := strings.Builder{}
	r := rune(s[0])
	previous := &r
	for i := 1; i < len(s); i++ {
		if previous != nil && isOpposite(rune(s[i]), *previous) {
			previous = nil
			continue
		}
		if previous != nil {
			sb.WriteRune(*previous)
		}
		r := rune(s[i])
		previous = &r
	}
	if previous != nil {
		sb.WriteRune(*previous)
	}
	return sb.String()
}

func isOpposite(r1, r2 rune) bool {
	return r1 != r2 && unicode.ToUpper(r1) == unicode.ToUpper(r2)
}

func fs2(input io.Reader) int {
	s := lib.ReaderToString(input)
	miner := lib.NewMiner()
	for r := 'a'; r <= 'z'; r++ {
		s2 := remove(s, r)
		miner.Add(reactions(s2))
	}
	return miner.Get()
}

func remove(s string, r rune) string {
	r2 := unicode.ToUpper(r)
	sb := strings.Builder{}
	for i := 0; i < len(s); i++ {
		cur := rune(s[i])
		if cur != r && cur != r2 {
			sb.WriteRune(cur)
		}
	}
	return sb.String()
}

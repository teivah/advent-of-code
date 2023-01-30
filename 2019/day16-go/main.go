package main

import (
	"fmt"
	"io"
	"strings"

	lib "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader, phases int) string {
	s := lib.ReaderToString(input)

	pattern := []int{0, 1, 0, -1}
	for phase := 0; phase < phases; phase++ {
		sb := strings.Builder{}
		for i := 0; i < len(s); i++ {
			fmt.Println(i, len(s))
			r := to(pattern, i, s)
			sb.WriteRune(r)
		}
		s = sb.String()
	}
	return s[:8]
}

func to(pattern []int, i int, s string) rune {
	sum := 0
	mult := pattern[0]
	patternIdx := 0
	if i == 0 {
		mult = pattern[1]
		patternIdx = 1
	}
	repeat := i
	for j := 0; j < len(s); j++ {
		v := lib.RuneToInt(rune(s[j])) * mult
		sum += v
		repeat--
		if repeat <= 0 {
			patternIdx = (patternIdx + 1) % len(pattern)
			mult = pattern[patternIdx]
			repeat = i + 1
		}
	}
	return lib.IntToRune(lib.Abs(sum % 10))
}

func fs2(input io.Reader, phases, repeat int) string {
	s := lib.ReaderToString(input)
	//size := len(s)
	offset := lib.StringToInt(s[:7])

	pattern := []int{0, 1, 0, -1}
	s = strings.Repeat(s, repeat)

	for phase := 0; phase < phases; phase++ {
		sb := strings.Builder{}
		for i := 0; i < len(s); i++ {
			r := to(pattern, i, s)
			sb.WriteRune(r)
		}
		s = sb.String()
	}

	//before := 0
	//for i := 0; i < repeat; i++ {
	//	x := s[before : before+size]
	//	fmt.Println(i, x[offset:offset+8])
	//	before = before + size
	//}

	return s[offset : offset+8]
	//return ""
}

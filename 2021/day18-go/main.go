package main

import (
	"bufio"
	"fmt"
	"io"
	"strconv"
	"strings"

	aoc "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
		line := scanner.Text()
		pair, _ := parse(line, 1)
		_ = pair
	}

	return 42
}

func add(s1, s2 string) string {
	return fmt.Sprintf("[%s,%s]")
}

func explode(s string) string {
	res := ""

	open := 0
	for i := 0; i < len(s); i++ {
		if s[i] == '[' {
			open++
			if open == 5 {
				r1 := rune(s[i+1])
				r2 := rune(s[i+3])
				if aoc.IsRuneDecimal(r1) && aoc.IsRuneDecimal(r2) {
					// Explode
					x := aoc.RuneToInt(r1)
					y := aoc.RuneToInt(r2)
					s = transformString(s, i, i+5, "0")

					for left := i - 1; left >= 0; left-- {
						r := rune(s[left])
						if aoc.IsRuneDecimal(r) {
							v := strconv.Itoa(aoc.RuneToInt(r) + x)
							s = transformString(s, left, left+1, v)
							break
						}
					}

					for right := i + 1; right < len(s); right++ {
						r := rune(s[right])
						if aoc.IsRuneDecimal(r) {
							v := strconv.Itoa(aoc.RuneToInt(r) + y)
							s = transformString(s, right, right+1, v)
							break
						}
					}

					return s
				}
			}
		} else if s[i] == ']' {
			open--
		}
	}

	return res
}

func transformString(s string, from, to int, res string) string {
	sb := strings.Builder{}
	for i := 0; i < from; i++ {
		sb.WriteByte(s[i])
	}
	for i := 0; i < len(res); i++ {
		sb.WriteByte(res[i])
	}
	for i := to; i < len(s); i++ {
		sb.WriteByte(s[i])
	}
	return sb.String()
}

func parse(s string, i int) (*Pair, int) {
	p := &Pair{}
	if s[i] == '[' {
		pair, j := parse(s, i+1)
		p.leftPair = pair
		i = j + 1
	} else {
		p.leftNumber = aoc.RuneToInt(rune(s[i]))
		i += 2
	}

	if s[i] == '[' {
		pair, j := parse(s, i+1)
		p.rightPair = pair
		i = j + 1
	} else {
		p.rightNumber = aoc.RuneToInt(rune(s[i]))
		i += 2
	}
	return p, i
}

type Pair struct {
	leftNumber  int
	leftPair    *Pair
	rightNumber int
	rightPair   *Pair
}

func (p *Pair) String() string {
	s := "["
	if p.leftPair == nil {
		s += strconv.Itoa(p.leftNumber)
	} else {
		s += p.leftPair.String()
	}
	s += ","
	if p.rightPair == nil {
		s += strconv.Itoa(p.rightNumber)
	} else {
		s += p.rightPair.String()
	}
	return s + "]"
}

//func explode(p *Pair) *Pair {
//	res := ""
//	s := p.String()
//	open := 0
//	for i := 0; i < len(s); i++ {
//		if s[i] ==
//	}
//}

func fs2(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
		line := scanner.Text()
		_ = line
	}

	return 42
}

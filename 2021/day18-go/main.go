package main

import (
	"fmt"
	"io"
	"math"
	"strconv"
	"strings"

	aoc "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) string {
	lines := aoc.ReaderToStrings(input)
	res := lines[0]
	for i := 1; i < len(lines); i++ {
		res = add(res, lines[i])
		res = reduction(res)
	}

	return res
}

func reduction(s string) string {
	for {
		v, change := explode(s)
		if change {
			s = v
			continue
		}

		v, change = split(s)
		if change {
			s = v
			continue
		}

		break
	}

	return s
}

func add(s1, s2 string) string {
	return fmt.Sprintf("[%s,%s]", s1, s2)
}

func split(s string) (string, bool) {
	for i := 1; i < len(s); i++ {
		if aoc.IsRuneDecimal(rune(s[i])) && aoc.IsRuneDecimal(rune(s[i-1])) {
			v := aoc.StringToInt(s[i-1 : i+1])
			return transformString(s, i-1, i+1, fmt.Sprintf("[%d,%d]",
				int(math.Floor(float64(v)/2.)), int(math.Ceil(float64(v)/2.)))), true
		}
	}
	return s, false
}

func explode(s string) (string, bool) {
	res := ""

	open := 0
	for i := 0; i < len(s); i++ {
		if s[i] == '[' {
			open++
			if open >= 5 {
				if aoc.IsRuneDecimal(rune(s[i+1])) {
					start := i
					j := i + 2
					for aoc.IsRuneDecimal(rune(s[j])) {
						j++
					}
					x := aoc.StringToInt(s[i+1 : j])

					if s[j] != ',' {
						continue
					}

					i = j + 1

					if aoc.IsRuneDecimal(rune(s[i])) {
						j := i + 1
						for aoc.IsRuneDecimal(rune(s[j])) {
							j++
						}
						y := aoc.StringToInt(s[i:j])

						// Explode

						s = transformString(s, start, j+1, "0")

						i = start
						for left := i - 1; left >= 0; left-- {
							if aoc.IsRuneDecimal(rune(s[left])) {
								j := left - 1
								for aoc.IsRuneDecimal(rune(s[j])) {
									j--
								}
								before := s[j+1 : left+1]
								n := aoc.StringToInt(before)
								v := strconv.Itoa(n + x)
								s = transformString(s, j+1, left+1, v)
								// Increment i

								if len(before) < len(v) {
									i += len(v) - len(before)
								}

								break
							}
						}

						for right := i + 1; right < len(s); right++ {
							if aoc.IsRuneDecimal(rune(s[right])) {
								j := right + 1
								for aoc.IsRuneDecimal(rune(s[j])) {
									j++
								}
								n := aoc.StringToInt(s[right:j])
								v := strconv.Itoa(n + y)
								s = transformString(s, right, j, v)
								break
							}
						}

						return s, true
					}
				}
			}
		} else if s[i] == ']' {
			open--
		}
	}

	return res, false
}

func magnitude(s string) int {
	p, _ := parse(s, 1)
	return p.magnitude()
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

func (p *Pair) magnitude() int {
	l := 0
	r := 0
	if p.leftPair == nil {
		l = p.leftNumber
	} else {
		l = p.leftPair.magnitude()
	}
	if p.rightPair == nil {
		r = p.rightNumber
	} else {
		r = p.rightPair.magnitude()
	}
	return 3*l + 2*r
}

type Pair struct {
	leftNumber  int
	leftPair    *Pair
	rightNumber int
	rightPair   *Pair
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

func fs2(input io.Reader) int {
	lines := aoc.ReaderToStrings(input)

	max := aoc.NewMaxer()
	for i := 0; i < len(lines); i++ {
		for j := i + 1; j < len(lines); j++ {
			res := add(lines[i], lines[j])
			res = reduction(res)
			max.Add(magnitude(res))

			res = add(lines[j], lines[i])
			res = reduction(res)
			max.Add(magnitude(res))
		}
	}

	return max.Get()
}

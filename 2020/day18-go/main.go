package main

import (
	"io"

	aoc "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	lines := aoc.ReaderToStrings(input)

	sum := 0
	for _, line := range lines {
		v, _ := calc(line, 0)
		sum += v
	}

	return sum
}

func calc(s string, i int) (value int, nextIndex int) {
	if i == len(s) {
		return 0, i
	}

	current := 0
	var operator *Operator
	for ; i < len(s); i++ {
		r := s[i]
		if r >= '0' && r <= '9' {
			j := i + 1
			for ; j < len(s); j++ {
				if s[j] >= '0' && s[j] <= '9' {
					continue
				}
				break
			}

			v := aoc.StringToInt(s[i:j])
			i = j - 1
			if operator == nil {
				current = v
			} else {
				switch *operator {
				case plus:
					current += v
				case mult:
					current *= v
				}
			}
		} else if r == '+' {
			v := plus
			operator = &v
		} else if r == '*' {
			v := mult
			operator = &v
		} else if r == '(' {
			v, last := calc(s, i+1)
			i = last
			if operator == nil {
				current = v
			} else {
				switch *operator {
				case plus:
					current += v
				case mult:
					current *= v
				}
			}
		} else if r == ')' {
			return current, i
		} else if r == ' ' {

		} else {
			panic(string(r))
		}
	}

	return current, i
}

type Operator int

const (
	plus Operator = iota
	mult Operator = iota
)

func fs2(input io.Reader) int {
	lines := aoc.ReaderToStrings(input)

	sum := 0
	for _, line := range lines {
		line = addParentheses(line)
		v, _ := calc(line, 0)
		sum += v
	}

	return sum
}

func addParentheses(s string) string {
	for i := 0; i < len(s); i++ {
		if s[i] == '+' {
			previous := findPrevious(s, i-2)
			next := findNext(s, i+2)
			s = app(s, previous, '(')
			s = app(s, next+1, ')')
			i++
		}
	}
	return s
}

func findPrevious(s string, i int) int {
	for ; i >= 0; i-- {
		if s[i] >= '0' && s[i] <= '9' {
			i--
			for ; i >= 0; i-- {
				if s[i] >= '0' && s[i] <= '9' {
					continue
				}
				break
			}
			return i + 1
		} else if s[i] == ' ' {

		} else if s[i] == ')' {
			closed := 1
			i--
			for ; i >= 0; i-- {
				if s[i] == ')' {
					closed++
				} else if s[i] == '(' {
					closed--
				}
				if closed == 0 {
					return i
				}
			}
		}
	}

	return 0
}

func findNext(s string, i int) int {
	for ; i < len(s); i++ {
		if s[i] >= '0' && s[i] <= '9' {
			i++
			for ; i < len(s); i++ {
				if s[i] >= '0' && s[i] <= '9' {
					continue
				}
				break
			}
			return i
		} else if s[i] == ' ' {

		} else if s[i] == '(' {
			opened := 1
			i++
			for ; i < len(s); i++ {
				if s[i] == '(' {
					opened++
				} else if s[i] == ')' {
					opened--
				}
				if opened == 0 {
					return i
				}
			}
		}
	}

	return len(s) + 1
}

func app(s string, i int, r rune) string {
	if i == 0 {
		return string(r) + s
	}
	if i == len(s) {
		return s + string(r)
	}
	return s[:i] + string(r) + s[i:]
}

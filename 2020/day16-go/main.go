package main

import (
	"io"
	"strings"

	aoc "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	lines := aoc.ReaderToStrings(input)

	rules, _, options := parse(lines)

	sum := 0
	for _, values := range options {
		for _, value := range values {
			valid := false
		outer:
			for _, windows := range rules {
				for _, window := range windows {
					if window.isValid(value) {
						valid = true
						break outer
					}
				}
			}

			if !valid {
				sum += value
			}
		}
	}

	return sum
}

func parse(lines []string) (map[string][]Window, []int, [][]int) {
	rules := make(map[string][]Window)
	i := 0
	for ; i < len(lines); i++ {
		s := lines[i]
		if s == "" {
			break
		}

		idx := strings.Index(s, ":")
		name := s[:idx]

		s = s[idx+2:]

		del := aoc.NewDelimiter(s, " or ")

		rules[name] = append(rules[name], toWindow(del.GetString(0)))
		rules[name] = append(rules[name], toWindow(del.GetString(1)))
	}

	i += 2
	ticket := parseInts(lines[i])

	i += 3
	var options [][]int
	for ; i < len(lines); i++ {
		options = append(options, parseInts(lines[i]))
	}

	return rules, ticket, options
}

func parseInts(s string) []int {
	del := aoc.NewDelimiter(s, ",")
	w := del.GetStrings()
	return aoc.StringsToInts(w)
}

type Window struct {
	from int
	to   int
}

func (w Window) isValid(n int) bool {
	return n >= w.from && n <= w.to
}

func toWindow(s string) Window {
	del := aoc.NewDelimiter(s, "-")
	return Window{
		from: del.GetInt(0),
		to:   del.GetInt(1),
	}
}

func fs2(input io.Reader) int {
	lines := aoc.ReaderToStrings(input)

	rules, ticket, options := parse(lines)

	invalidTickets := make(map[int]bool)
	for optionIdx, values := range options {
		for _, value := range values {
			valid := false
		outer:
			for _, windows := range rules {
				for _, window := range windows {
					if window.isValid(value) {
						valid = true
						break outer
					}
				}
			}

			if !valid {
				invalidTickets[optionIdx] = true
				break
			}
		}
	}

	possibilities := make(map[int]map[string]struct{})

	for i := 0; i < len(options[0]); i++ {
		remainingClasses := make(map[string]bool)
		for name := range rules {
			remainingClasses[name] = true
		}

		for ticketID, values := range options {
			if invalidTickets[ticketID] {
				continue
			}

			// Option
			value := values[i]
			for className := range remainingClasses {
				rule := rules[className]

				valid := false
				for _, window := range rule {
					if window.isValid(value) {
						valid = true
						break
					}
				}

				if !valid {
					delete(remainingClasses, className)
				}
			}

			// Ticket
			value = ticket[i]
			for className := range remainingClasses {
				rule := rules[className]

				valid := false
				for _, window := range rule {
					if window.isValid(value) {
						valid = true
						break
					}
				}

				if !valid {
					delete(remainingClasses, className)
				}
			}
		}

		possibilities[i] = make(map[string]struct{})
		for name := range remainingClasses {
			possibilities[i][name] = struct{}{}
		}
	}

	inDegree := make(map[int]int)
	var q []string
	for k, v := range possibilities {
		inDegree[k] = len(v)
		if len(v) == 1 {
			q = append(q, getName(v))
		}
	}

	for len(q) != 0 {
		found := q[0]
		q = q[1:]

		for _, opts := range possibilities {
			if len(opts) == 1 {
				continue
			}
			delete(opts, found)

			if len(opts) == 1 {
				q = append(q, getName(opts))
			}
		}
	}

	res := 1
	for rowID, names := range possibilities {
		name := getName(names)

		if strings.Contains(name, "departure") {
			res *= ticket[rowID]
		}
	}

	return res
}

func getName(m map[string]struct{}) string {
	for k := range m {
		return k
	}
	panic("not found")
}

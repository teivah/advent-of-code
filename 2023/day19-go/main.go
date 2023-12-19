package main

import (
	"bufio"
	"io"
	"strings"

	aoc "github.com/teivah/advent-of-code"
)

const startingWorkflow = "in"

type Result int

const (
	accepted Result = iota
	rejected
	sent
)

type Rating map[string]int

type workflowFunc func(Rating) (bool, Result, string)

func fs1(input io.Reader) int {
	groups := aoc.StringGroups(aoc.ReaderToStrings(input))
	workflows := parseWorkflows(groups[0])
	ratings := parseRatings(groups[1])

	res := 0
outer:
	for _, rating := range ratings {
		workflow := startingWorkflow

	inner:
		for {
			steps := workflows[workflow]

			for _, step := range steps {
				matching, result, next := step(rating)
				if !matching {
					continue
				}
				switch result {
				case rejected:
					continue outer
				case accepted:
					res += rating["x"] + rating["m"] + rating["a"] + rating["s"]
					continue outer
				case sent:
					workflow = next
					continue inner
				default:
					panic(result)
				}
			}
		}
	}
	return res
}

func parseWorkflows(lines []string) map[string][]workflowFunc {
	m := make(map[string][]workflowFunc)
	for _, line := range lines {
		sep := strings.Index(line, "{")
		workflowName := line[:sep]
		var workflows []workflowFunc
		s := line[sep+1 : len(line)-1]

		del := aoc.NewDelimiter(s, ",")
		for _, step := range del.GetStrings() {
			f := parseWorkflowStep(step)
			workflows = append(workflows, f)
		}
		m[workflowName] = workflows
	}
	return m
}

func parseWorkflowStep(s string) workflowFunc {
	thenIdx := strings.Index(s, ":")

	var res Result
	var next string

	if thenIdx == -1 {
		switch s {
		case "A":
			res = accepted
		case "R":
			res = rejected
		default:
			res = sent
			next = s
		}
		return func(_ Rating) (bool, Result, string) {
			return true, res, next
		}
	}

	beforeThen := s[:thenIdx]
	next = s[thenIdx+1:]
	operatorIdx := 0
	var isSmaller bool
	if v := strings.Index(beforeThen, "<"); v != -1 {
		isSmaller = true
		operatorIdx = v
	} else if v := strings.Index(beforeThen, ">"); v != -1 {
		operatorIdx = v
	} else {
		panic(s)
	}

	workflowName := beforeThen[:operatorIdx]
	condition := aoc.StringToInt(beforeThen[operatorIdx+1:])

	if next != "A" && next != "R" {
		if isSmaller {
			return func(r Rating) (bool, Result, string) {
				if r[workflowName] < condition {
					return true, sent, next
				}
				return false, 0, ""
			}
		}
		return func(r Rating) (bool, Result, string) {
			if r[workflowName] > condition {
				return true, sent, next
			}
			return false, 0, ""
		}
	}

	if next == "A" {
		if isSmaller {
			return func(r Rating) (bool, Result, string) {
				if r[workflowName] < condition {
					return true, accepted, ""
				}
				return false, 0, ""
			}
		}
		return func(r Rating) (bool, Result, string) {
			if r[workflowName] > condition {
				return true, accepted, ""
			}
			return false, 0, ""
		}
	}

	if next == "R" {
		if isSmaller {
			return func(r Rating) (bool, Result, string) {
				if r[workflowName] < condition {
					return true, rejected, ""
				}
				return false, 0, ""
			}
		}
		return func(r Rating) (bool, Result, string) {
			if r[workflowName] > condition {
				return true, rejected, ""
			}
			return false, 0, ""
		}
	}

	panic("not")
}

func parseRatings(lines []string) []Rating {
	ratings := make([]Rating, 0, len(lines))
	for _, line := range lines {
		// Remove { and }
		line = line[1 : len(line)-1]
		del := aoc.NewDelimiter(line, ",")
		strings := del.GetStrings()
		rating := make(map[string]int, len(strings))
		for _, s := range strings {
			name, value := parseRating(s)
			rating[name] = value
		}
		ratings = append(ratings, rating)
	}
	return ratings
}

func parseRating(s string) (string, int) {
	del := aoc.NewDelimiter(s, "=")
	return del.GetString(0), del.GetInt(1)
}

func fs2(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
		line := scanner.Text()
		_ = line
	}

	return 42
}

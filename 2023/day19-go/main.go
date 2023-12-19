package main

import (
	"fmt"
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

type Condition int

const (
	alwaysTrue Condition = iota
	smaller
	greater
)

type Rating map[string]int

type Step struct {
	f        stepFunc
	res      Result
	variable string
	cond     Condition
	value    int
}

type stepFunc func(Rating) (bool, Result, string)

func fs1(input io.Reader) int {
	groups := aoc.StringGroups(aoc.ReaderToStrings(input))
	workflows := parseWorkflows(groups[0])
	ratings := parseRatings(groups[1])

	fmt.Println(workflows)

	res := 0
outer:
	for _, rating := range ratings {
		workflow := startingWorkflow

	inner:
		for {
			steps := workflows[workflow]

			for _, step := range steps {
				matching, result, next := step.f(rating)
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

func parseWorkflows(lines []string) map[string][]Step {
	m := make(map[string][]Step)
	for _, line := range lines {
		sep := strings.Index(line, "{")
		workflowName := line[:sep]
		var workflows []Step
		s := line[sep+1 : len(line)-1]

		del := aoc.NewDelimiter(s, ",")
		for _, stepName := range del.GetStrings() {
			step := parseWorkflowStep(stepName)
			workflows = append(workflows, step)
		}
		m[workflowName] = workflows
	}
	return m
}

func parseWorkflowStep(s string) Step {
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
		return Step{
			f: func(_ Rating) (bool, Result, string) {
				return true, res, next
			},
			res:  res,
			cond: alwaysTrue,
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

	variable := beforeThen[:operatorIdx]
	value := aoc.StringToInt(beforeThen[operatorIdx+1:])

	switch next {
	case "A":
		res = accepted
	case "R":
		res = rejected
	default:
		res = sent
	}

	if isSmaller {
		return Step{
			f: func(r Rating) (bool, Result, string) {
				if r[variable] < value {
					return true, res, next
				}
				return false, res, ""
			},
			res:      res,
			variable: variable,
			cond:     smaller,
			value:    value,
		}
	}
	return Step{
		f: func(r Rating) (bool, Result, string) {
			if r[variable] > value {
				return true, res, next
			}
			return false, 0, ""
		},
		res:      res,
		variable: variable,
		cond:     greater,
		value:    value,
	}
}

func parseRatings(lines []string) []Rating {
	ratings := make([]Rating, 0, len(lines))
	for _, line := range lines {
		// Remove { and }
		line = line[1 : len(line)-1]
		del := aoc.NewDelimiter(line, ",")
		items := del.GetStrings()
		rating := make(map[string]int, len(items))
		for _, s := range items {
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
	//groups := aoc.StringGroups(aoc.ReaderToStrings(input))
	//workflows := parseWorkflows(groups[0])
	//ratings := parseRatings(groups[1])

	return 42
}

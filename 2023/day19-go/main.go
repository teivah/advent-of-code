package main

import (
	"io"
	"strings"

	aoc "github.com/teivah/advent-of-code"
)

const acceptedStr = "A"
const rejectedStr = "R"

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
	f            stepFunc
	res          Result
	condVariable string
	cond         Condition
	condValue    int
	next         string
}

type stepFunc func(Rating) (bool, Result, string)

func fs1(input io.Reader) int {
	groups := aoc.StringGroups(aoc.ReaderToStrings(input))
	workflows := parseWorkflows(groups[0])
	ratings := parseRatings(groups[1])

	//fmt.Println(workflows)

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
			next: next,
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
	case acceptedStr:
		res = accepted
	case rejectedStr:
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
			res:          res,
			condVariable: variable,
			cond:         smaller,
			condValue:    value,
			next:         next,
		}
	}
	return Step{
		f: func(r Rating) (bool, Result, string) {
			if r[variable] > value {
				return true, res, next
			}
			return false, 0, ""
		},
		res:          res,
		condVariable: variable,
		cond:         greater,
		condValue:    value,
		next:         next,
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
	groups := aoc.StringGroups(aoc.ReaderToStrings(input))
	workflows := parseWorkflows(groups[0])
	_ = workflows

	parent := dependency(workflows, startingWorkflow)
	defaultRange := Range{
		from: 1,
		to:   4000,
	}
	intervals := dfs(parent, RangeRating{
		"x": defaultRange,
		"m": defaultRange,
		"a": defaultRange,
		"s": defaultRange,
	})

	sum := 0
	for _, interval := range intervals {
		v := 1
		for _, r := range interval {
			v *= r.to - r.from + 1
		}
		sum += v
	}
	return sum
}

type Node struct {
	accepted bool
	rejected bool
	name     string
	children []*Node
	steps    []Step
}

type RangeRating map[string]Range

func (r RangeRating) clone() RangeRating {
	res := make(map[string]Range, len(r))
	for k, v := range r {
		res[k] = v
	}
	return res
}

type Range struct {
	from int
	to   int
}

func dependency(workflows map[string][]Step, workflowName string) *Node {
	var children []*Node
	var steps []Step
	for _, step := range workflows[workflowName] {
		switch step.res {
		case accepted:
			children = append(children, &Node{
				accepted: true,
			})
			steps = append(steps, step)
			continue
		case rejected:
			children = append(children, &Node{
				rejected: true,
			})
			steps = append(steps, step)
			continue
		}

		n := dependency(workflows, step.next)
		if n != nil {
			children = append(children, n)
			steps = append(steps, step)
		}
	}

	return &Node{
		name:     workflowName,
		children: children,
		steps:    steps,
	}
}

func dfs(node *Node, r RangeRating) []RangeRating {
	if node.accepted {
		for _, v := range r {
			if v.to < v.from {
				return nil
			}
		}
		return []RangeRating{r}
	}
	if node.rejected {
		return nil
	}

	parent := r.clone()
	var res []RangeRating
	for i, child := range node.children {
		r := parent.clone()

		step := node.steps[i]
		switch step.cond {
		case alwaysTrue:
			res = append(res, dfs(child, r)...)
		case smaller:
			variable := step.condVariable
			rr := r[variable]
			rr.to = min(rr.to, step.condValue-1)
			r[variable] = rr
			res = append(res, dfs(child, r)...)

			rr = parent[variable]
			rr.from = max(rr.from, step.condValue)
			parent[variable] = rr
		case greater:
			variable := step.condVariable
			rr := r[variable]
			rr.from = max(rr.from, step.condValue+1)
			r[variable] = rr
			res = append(res, dfs(child, r)...)

			rr = parent[variable]
			rr.to = min(rr.to, step.condValue)
			parent[variable] = rr
		}
	}
	return res
}

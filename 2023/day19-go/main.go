package main

import (
	"fmt"
	"io"
	"sort"
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

	//workflow := startingWorkflow
	//outer:
	//	for _, interval := range intervals {
	//		for x := interval["x"].from; x <= interval["x"].to; x++ {
	//			for m := interval["m"].from; m <= interval["m"].to; m++ {
	//				for a := interval["a"].from; a <= interval["a"].to; a++ {
	//					for s := interval["s"].from; s <= interval["s"].to; s++ {
	//						rating := Rating{
	//							"x": x,
	//							"m": m,
	//							"a": a,
	//							"s": s,
	//						}
	//
	//					inner:
	//						for {
	//							steps := workflows[workflow]
	//
	//							for _, step := range steps {
	//								matching, result, next := step.f(rating)
	//								if !matching {
	//									continue
	//								}
	//								switch result {
	//								case rejected:
	//									panic(fmt.Sprintf("%v", rating))
	//								case accepted:
	//									continue outer
	//								case sent:
	//									workflow = next
	//									continue inner
	//								default:
	//									panic(result)
	//								}
	//							}
	//						}
	//					}
	//				}
	//			}
	//		}
	//	}

	for _, interval := range intervals {
		fmt.Println(interval)
	}

	endpoints := make(map[string]map[int]struct{})
	for _, k := range []string{"x", "m", "a", "s"} {
		m := make(map[int]struct{})
		for _, interval := range intervals {
			r := interval[k]
			m[r.from] = struct{}{}
			m[r.to] = struct{}{}
		}
		m[1] = struct{}{}
		m[4000] = struct{}{}
		endpoints[k] = m
	}

	ordered := make(map[string][]int)
	for k, m := range endpoints {
		var s []int
		for k := range m {
			s = append(s, k)
		}
		sort.Ints(s)
		ordered[k] = s
	}

	fmt.Println()
	fmt.Println("ordered")
	for k, v := range ordered {
		fmt.Println(k, v)
	}
	fmt.Println()

	var keys []map[string][2]int
	for _, interval := range intervals {
		key := make(map[string][2]int)
		for _, k := range []string{"x", "m", "a", "s"} {
			r := interval[k]
			from := r.from
			to := r.to

			ints := ordered[k]
			startIdx := 0
			endIdx := 0
			for idx, i := range ints {
				if i == from {
					startIdx = idx
				}
				if i == to {
					if i == 4000 {
						endIdx = idx + 1
					} else {
						endIdx = idx
					}
					break
				}
			}
			key[k] = [2]int{startIdx, endIdx}
		}
		keys = append(keys, key)
	}

	type entry struct {
		x int
		m int
		a int
		s int
	}

	//for _, key := range keys {
	//	fmt.Println(key)
	//}

	cache := make(map[entry]bool)
	res := 0
	for i, key := range keys {
		fmt.Println(i, len(keys))
		x := key["x"]
		m := key["m"]
		a := key["a"]
		s := key["s"]

		specialX := false
		if x[0] == x[1] {
			specialX = true
			x[1]++
		}
		specialM := false
		if m[0] == m[1] {
			specialM = true
			m[1]++
		}
		specialA := false
		if a[0] == a[1] {
			specialA = true
			a[1]++
		}
		specialS := false
		if s[0] == s[1] {
			specialS = true
			s[1]++
		}

		for iX := x[0]; iX < x[1]; iX++ {
			fromX := ordered["x"][iX]
			toX := 0
			if specialX {
				toX = fromX
			} else {
				if iX+1 == len(ordered["x"]) {
					toX = 4000
				} else {
					toX = ordered["x"][iX+1]
				}
			}

			for iM := m[0]; iM < m[1]; iM++ {
				fromM := ordered["m"][iM]
				toM := 0
				if specialM {
					toM = fromM
				} else {
					if iM+1 == len(ordered["m"]) {
						toM = 4000
					} else {
						toM = ordered["m"][iM+1]
					}
				}

				for iA := a[0]; iA < a[1]; iA++ {
					fromA := ordered["a"][iA]
					toA := 0
					if specialA {
						toA = fromA
					} else {
						if iA+1 == len(ordered["a"]) {
							toA = 4000
						} else {
							toA = ordered["a"][iA+1]
						}
					}

					for iS := s[0]; iS < s[1]; iS++ {
						fromS := ordered["s"][iS]
						toS := 0
						if specialS {
							toS = fromS
						} else {
							if iS+1 == len(ordered["s"]) {
								toS = 4000
							} else {
								toS = ordered["s"][iS+1]
							}
						}

						e := entry{
							x: toX,
							m: toM,
							a: toA,
							s: toS,
						}

						if cache[e] {
							continue
						}
						fmt.Println(e)
						cache[e] = true

						res += (toX - fromX + 1) * (toM - fromM + 1) * (toA - fromA + 1) * (toS - fromS + 1)
					}
				}
			}
		}
	}
	return res

	// Overlaps
	//total := 0
	//for _, interval := range intervals {
	//	fmt.Println(interval)
	//	res := 1
	//	for _, v := range interval {
	//		res *= v.to - v.from + 1
	//	}
	//	total += res
	//}
	//
	//overlap := 0
	//for i := 0; i < len(intervals); i++ {
	//	for j := i + 1; j < len(intervals); j++ {
	//		overlap += getOverlapping(intervals[i], intervals[j])
	//	}
	//}
	// return total - overlap

	// Segment tree
	//var parents []*SegmentTreeNode
	//for _, interval := range intervals {
	//	parents = segmentTree([]string{"x", "m", "a", "s"}, interval, parents)
	//}

	return 0
}

type SegmentTreeNode struct {
	// Included
	from int
	// Included
	to       int
	children []*SegmentTreeNode
}

func segmentTree(keys []string, intervals RangeRating, nodes []*SegmentTreeNode) []*SegmentTreeNode {
	if len(keys) == 0 {
		return nil
	}

	interval := intervals[keys[0]]
	keys = keys[1:]

	if len(nodes) == 0 {
		node := &SegmentTreeNode{
			from: interval.from,
			to:   interval.to,
		}
		node.children = append(node.children, segmentTree(keys, intervals, nil)...)
		return []*SegmentTreeNode{node}
	}
	return nil
}

func getOverlapping(i1, i2 RangeRating) int {
	// map[a:{0 2005} m:{0 4000} s:{0 1350} x:{0 1415}]
	// map[a:{0 2005} m:{0 4000} s:{0 1350} x:{2663 4000}]
	defaultRange := Range{
		from: 1,
		to:   4000,
	}
	r := RangeRating{
		"x": defaultRange,
		"m": defaultRange,
		"a": defaultRange,
		"s": defaultRange,
	}
	for k, r1 := range i1 {
		r2 := i2[k]

		if (r1.from <= r2.from && r2.from <= r1.to) ||
			(r2.from <= r1.from && r1.from <= r2.to) {
			v := r[k]
			v.from = max(v.from, r1.from, r2.from)
			v.to = min(v.to, r1.to, r2.to)
			r[k] = v
		} else {
			return 0
		}
	}

	res := 1
	for _, v := range r {
		if v.from > v.to {
			return 0
		}
		res *= v.to - v.from + 1
	}
	return res
}

type Node struct {
	accepted bool
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

func dfs(node *Node, r RangeRating) []RangeRating {
	if node.accepted {
		for _, v := range r {
			if v.to < v.from {
				return nil
			}
		}
		return []RangeRating{r}
	}

	var res []RangeRating
	for i, child := range node.children {
		r := r.clone()

		step := node.steps[i]
		switch step.cond {
		case alwaysTrue:
			res = append(res, dfs(child, r)...)
		case smaller:
			variable := step.condVariable
			rr := r[variable]
			rr.to = step.condValue - 1
			r[variable] = rr
			res = append(res, dfs(child, r)...)
		case greater:
			variable := step.condVariable
			rr := r[variable]
			rr.from = step.condValue + 1
			r[variable] = rr
			res = append(res, dfs(child, r)...)
		}
	}
	return res
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

func topologicalSort(workflows map[string][]Step) []string {
	inDegree := make(map[string]int)
	for workflow := range workflows {
		inDegree[workflow] = 0
	}

	for _, steps := range workflows {
		for _, step := range steps {
			vertex := ""
			switch step.res {
			case accepted:
				vertex = acceptedStr
			case rejected:
				vertex = rejectedStr
			case sent:
				vertex = step.next
			}
			inDegree[vertex]++
		}
	}

	var q []string
	for vertex, factor := range inDegree {
		if factor == 0 {
			q = append(q, vertex)
		}
	}

	var res []string
	for len(q) != 0 {
		vertex := q[0]
		q = q[1:]
		res = append(res, vertex)

		for _, step := range workflows[vertex] {
			child := ""
			switch step.res {
			case accepted:
				child = acceptedStr
			case rejected:
				child = rejectedStr
			case sent:
				child = step.next
			}
			inDegree[child]--
			if inDegree[child] == 0 {
				q = append(q, child)
			}
		}
	}

	if len(res) != len(workflows)+2 { // Adding accepted and rejected
		panic("invalid")
	}
	return res
}

package main

import (
	"io"
	"regexp"

	aoc "github.com/teivah/advent-of-code"
)

var (
	re = regexp.MustCompile(`mul\((\d{1,3}),(\d{1,3})\)`)
)

func fs1(input io.Reader) int {
	lines := aoc.ReaderToStrings(input)
	res := 0
	for _, line := range lines {
		matches := re.FindAllStringSubmatch(line, -1)
		for _, match := range matches {
			res += aoc.StringToInt(match[1]) * aoc.StringToInt(match[2])
		}
	}
	return res
}

func fs2(input io.Reader) int {
	lines := aoc.ReaderToStrings(input)

	pq := aoc.NewPriorityQueue[elem](func(a, b elem) bool {
		return a.position < b.position
	})
	res := 0
	curDo := true
	for _, line := range lines {
		for _, v := range aoc.FindStringIndices(line, "do()") {
			pq.Push(elem{
				position: v,
				do:       true,
			})
		}
		for _, v := range aoc.FindStringIndices(line, "don't()") {
			pq.Push(elem{
				position: v,
				dont:     true,
			})
		}

		for _, match := range aoc.RegexpFindSubmatches(line, re) {
			m := mul{
				x: aoc.StringToInt(line[match.CapturingGroups[0].Start:match.CapturingGroups[0].End]),
				y: aoc.StringToInt(line[match.CapturingGroups[1].Start:match.CapturingGroups[1].End]),
			}
			pq.Push(elem{
				position: match.Start,
				mul:      true,
				vmul:     m,
			})
		}

		for !pq.IsEmpty() {
			e := pq.Pop()
			switch {
			case e.do:
				curDo = true
			case e.dont:
				curDo = false
			case e.mul:
				if curDo {
					res += e.vmul.x * e.vmul.y
				}
			}
		}
	}
	return res
}

type elem struct {
	position int
	do       bool
	dont     bool
	mul      bool
	vmul     mul
}

type mul struct {
	x int
	y int
}

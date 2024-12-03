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

	pq := aoc.NewPriorityQueue[elem](func(a, b elem) int {
		return a.position - b.position
	})
	res := 0
	curDo := true
	for _, line := range lines {
		do := aoc.IndexAll(line, "do()")
		for _, v := range do {
			pq.Push(elem{
				position: v,
				do:       true,
			})
		}
		dont := aoc.IndexAll(line, "don't()")
		for _, v := range dont {
			pq.Push(elem{
				position: v,
				dont:     true,
			})
		}

		matches := re.FindAllStringSubmatchIndex(line, -1)
		for _, match := range matches {
			m := mul{
				x: aoc.StringToInt(line[match[2]:match[3]]),
				y: aoc.StringToInt(line[match[4]:match[5]]),
			}
			pq.Push(elem{
				position: match[0],
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

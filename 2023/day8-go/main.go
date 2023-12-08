package main

import (
	"io"

	aoc "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	groups := aoc.StringGroups(aoc.ReaderToStrings(input))
	directions := parseDirections(groups[0][0])
	nodes := parseNodes(groups[1])

	cur := "AAA"
	distance := 0
	for i := 0; ; i = (i + 1) % len(directions) {
		if cur == "ZZZ" {
			return distance
		}
		dir := directions[i]
		if dir == aoc.Left {
			cur = nodes[cur].left
		} else {
			cur = nodes[cur].right
		}
		distance++
	}
}

func parseDirections(s string) []aoc.Direction {
	var instructions []aoc.Direction
	for _, c := range s {
		switch c {
		case 'L':
			instructions = append(instructions, aoc.Left)
		case 'R':
			instructions = append(instructions, aoc.Right)
		default:
			panic(c)
		}
	}
	return instructions
}

type Node struct {
	left  string
	right string
}

func parseNodes(lines []string) map[string]Node {
	m := make(map[string]Node, len(lines))
	for _, s := range lines {
		node, name := parseNode(s)
		m[name] = node
	}
	return m
}

func parseNode(s string) (Node, string) {
	del := aoc.NewDelimiter(s, " = ")
	name := del.GetString(0)

	nodes := del.GetString(1)
	nodes = nodes[1 : len(nodes)-1]
	directions := aoc.NewDelimiter(nodes, ", ")
	return Node{
		left:  directions.GetString(0),
		right: directions.GetString(1),
	}, name
}

func fs2(input io.Reader) int {
	groups := aoc.StringGroups(aoc.ReaderToStrings(input))
	directions := parseDirections(groups[0][0])
	nodes := parseNodes(groups[1])

	var curs []string
	for name := range nodes {
		if isStartingNode(name) {
			curs = append(curs, name)
		}
	}

	distance := 0
	distances := make(map[int]int)
	for i := 0; ; i = (i + 1) % len(directions) {
		if len(distances) == len(curs) {
			break
		}

		dir := directions[i]
		for curIdx, name := range curs {
			if dir == aoc.Left {
				curs[curIdx] = nodes[name].left
			} else {
				curs[curIdx] = nodes[name].right
			}
		}
		distance++

		for idx, cur := range curs {
			if isEndingNode(cur) {
				if _, exists := distances[idx]; exists {
					continue
				}
				distances[idx] = distance
			}
		}
	}

	numbers := make([]int, 0, len(curs))
	for _, d := range distances {
		numbers = append(numbers, d)
	}
	return aoc.LeastCommonMultiple(numbers)
}

func isStartingNode(name string) bool {
	return name[len(name)-1] == 'A'
}

func isEndingNode(name string) bool {
	return name[len(name)-1] == 'Z'
}

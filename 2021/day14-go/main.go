package main

import (
	"fmt"
	"io"

	aoc "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader, steps int) int {
	groups := aoc.StringGroups(aoc.ReaderToStrings(input))

	template := groups[0][0]
	transformations := make(map[string]rune)
	for _, line := range groups[1] {
		del := aoc.NewDelimiter(line, " -> ")
		transformations[del.GetString(0)] = rune(del.GetString(1)[0])
	}

	head := &Node{
		r: rune(template[0]),
	}
	cur := head
	for i := 1; i < len(template); i++ {
		n := &Node{
			r:        rune(template[i]),
			previous: cur,
		}
		cur.next = n
		cur = n
	}

	for i := 0; i < steps; i++ {
		transform(head, transformations)
	}

	return getDifference(head)
}

func transform(cur *Node, transformations map[string]rune) {
	previous := cur.r
	cur = cur.next

	for cur != nil {
		s := fmt.Sprintf("%c%c", previous, cur.r)

		res, contains := transformations[s]
		if contains {
			n := &Node{
				r:        res,
				previous: cur.previous,
				next:     cur,
			}
			cur.previous.next = n
			cur.previous = n
		}

		previous = cur.r
		cur = cur.next
	}
}

func getDifference(cur *Node) int {
	m := make(map[rune]int)
	for cur != nil {
		m[cur.r]++
		cur = cur.next
	}

	minMax := aoc.NewMinerMaxer()
	for _, v := range m {
		minMax.Add(v)
	}

	return minMax.GetMax() - minMax.GetMin()
}

type Node struct {
	r        rune
	previous *Node
	next     *Node
}

func fs2(input io.Reader, steps int) int {
	groups := aoc.StringGroups(aoc.ReaderToStrings(input))

	template := groups[0][0]
	transformations := make(map[string]rune)
	for _, line := range groups[1] {
		del := aoc.NewDelimiter(line, " -> ")
		transformations[del.GetString(0)] = rune(del.GetString(1)[0])
	}

	pairs := make(map[string]int)
	for i := 1; i < len(template); i++ {
		pairs[fmt.Sprintf("%c%c", template[i-1], template[i])]++
	}

	for i := 0; i < steps; i++ {
		pairs = transformPairs(pairs, transformations)
	}

	m := make(map[rune]int)
	for s, v := range pairs {
		m[rune(s[0])] += v
		m[rune(s[1])] += v
	}

	minMax := aoc.NewMinerMaxer()
	for _, v := range m {
		minMax.Add(v)
	}
	return (minMax.GetMax()-minMax.GetMin())/2 + 1
}

func transformPairs(pairs map[string]int, transformations map[string]rune) map[string]int {
	res := make(map[string]int, len(pairs))
	for k, count := range pairs {
		if dest, exists := transformations[k]; exists {
			pair1 := fmt.Sprintf("%c%c", rune(k[0]), dest)
			pair2 := fmt.Sprintf("%c%c", dest, rune(k[1]))
			res[pair1] += count
			res[pair2] += count
		} else {
			res[k] = count
		}
	}
	return res
}

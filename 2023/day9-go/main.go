package main

import (
	"bufio"
	"io"

	aoc "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	res := 0
	for scanner.Scan() {
		s := scanner.Text()
		del := aoc.NewDelimiter(s, " ")
		v := getResult1(del.GetInts())
		res += v
	}

	return res
}

func getResult1(numbers []int) int {
	var lines [][]int
	lines = append(lines, numbers)

	cur := numbers
	for {
		var next []int
		allZero := true
		for i := 0; i < len(cur)-1; i++ {
			diff := cur[i+1] - cur[i]
			if diff != 0 {
				allZero = false
			}
			next = append(next, diff)
		}

		lines = append(lines, next)
		if allZero {
			break
		}
		cur = next
	}

	lines[len(lines)-1] = append(lines[len(lines)-1], 0)

	for i := len(lines) - 2; i >= 0; i-- {
		sum := lines[i+1][len(lines[i+1])-1] + lines[i][len(lines[i])-1]
		lines[i] = append(lines[i], sum)
	}

	return lines[0][len(lines[0])-1]
}

func fs2(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	res := 0
	for scanner.Scan() {
		s := scanner.Text()
		del := aoc.NewDelimiter(s, " ")
		v := getResult2(del.GetInts())
		res += v
	}

	return res
}

type Node struct {
	value    int
	previous *Node
	next     *Node
}

func getResult2(numbers []int) int {
	var lines []*Node
	lines = append(lines, toNodes(numbers))

	cur := lines[0]
	for {
		var head *Node
		var curNode *Node
		allZero := true

		for cur.next != nil {
			diff := cur.value - cur.next.value
			if diff != 0 {
				allZero = false
			}

			n := &Node{
				value: diff,
			}
			if head == nil {
				head = n
				curNode = n
			} else {
				curNode.next = n
				curNode = n
			}
			cur = cur.next
		}

		lines = append(lines, head)
		if allZero {
			break
		}
		cur = head
	}

	lines[len(lines)-1].previous = &Node{
		value: 0,
	}
	lines[len(lines)-1] = lines[len(lines)-1].previous

	for i := len(lines) - 2; i >= 0; i-- {
		sum := lines[i].value + lines[i+1].value
		lines[i].previous = &Node{
			value: sum,
		}
		lines[i] = lines[i].previous
	}

	return lines[0].value
}

func toNodes(numbers []int) *Node {
	head := &Node{
		value: numbers[0],
	}
	cur := head
	for i := 1; i < len(numbers); i++ {
		n := &Node{
			value:    numbers[i],
			previous: cur,
		}
		cur.next = n
		cur = n
	}
	return head
}

package main

import (
	"fmt"
	"strconv"

	lib "github.com/teivah/advent-of-code"
)

func fs1(players, lastMarble int) int {
	board := []int{0, 1}
	scores := make([]int, players)

	i := 1
	for marble := 2; marble <= lastMarble; marble++ {
		if marble%23 == 0 {
			player := (marble - 1) % players
			scores[player] += marble
			j := lib.Mod(i-7, len(board))
			scores[player] += board[j]

			if j == 0 {
				board = board[1:]
			} else if j == len(board)-1 {
				board = board[:len(board)-1]
			} else {
				res := copy(board[:j])
				res = append(res, board[j+1:]...)
				board = res
			}
			i = j

			continue
		}

		i = (i + 1) % (len(board))

		if i == 0 {
			// Second
			res := []int{0, marble}
			res = append(res, board[1:]...)
			board = res
			i = 1
		} else if i == len(board)-1 {
			// Last
			board = append(board, marble)
			i++
		} else {
			// Middle
			res := copy(board[:i+1])
			res = append(res, marble)
			res = append(res, board[i+1:]...)
			board = res
			i++
		}
	}

	maxScore := lib.NewMaxer()
	for _, score := range scores {
		maxScore.Add(score)
	}

	return maxScore.Get()
}

func copy(s []int) []int {
	res := make([]int, len(s))
	for i, v := range s {
		res[i] = v
	}
	return res
}

func fs2(players, lastMarble int) int {
	scores := make([]int, players)

	current := &Node{value: 0}
	current.previous = current
	current.next = current
	for marble := 1; marble <= lastMarble; marble++ {
		if marble%23 == 0 {
			player := (marble - 1) % players
			scores[player] += marble

			for i := 0; i < 7; i++ {
				current = current.previous
			}

			scores[player] += current.value
			current.previous.next = current.next
			current.next.previous = current.previous
			current = current.next
			continue
		}

		node := &Node{value: marble, previous: current.next, next: current.next.next}
		current.next.next.previous = node
		current.next.next = node
		current = node
	}

	maxScore := lib.NewMaxer()
	for _, score := range scores {
		maxScore.Add(score)
	}

	return maxScore.Get()
}

type Node struct {
	value    int
	previous *Node
	next     *Node
}

func (n *Node) print() {
	s := strconv.Itoa(n.value)
	cur := n.next
	for cur != n {
		s += "," + strconv.Itoa(cur.value)
		cur = cur.next
	}
	fmt.Println(s)
}

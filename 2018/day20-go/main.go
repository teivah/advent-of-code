package main

import (
	"io"

	lib "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	regex := lib.ReaderToString(input)
	regex = regex[1 : len(regex)-1]

	current := &Room{}

	_ = traverse(regex, 0, current)

	return bfs(current)
}

func traverse(s string, i int, current *Room) []*Room {
	if i == len(s) {
		return nil
	}

	root := current
	var rooms []*Room
	for ; i < len(s); i++ {
		switch s[i] {
		case 'N':
			if current.north == nil {
				current.north = &Room{pos: current.pos.delta(-1, 0), south: current}
			}
			current = current.north
		case 'S':
			if current.south == nil {
				current.south = &Room{pos: current.pos.delta(1, 0), north: current}
			}
			current = current.south
		case 'W':
			if current.west == nil {
				current.west = &Room{pos: current.pos.delta(0, -1), east: current}
			}
			current = current.west
		case 'E':
			if current.east == nil {
				current.east = &Room{pos: current.pos.delta(0, 1), west: current}
			}
			current = current.east
		case '(':
			rooms = append(rooms, traverse(s, i+1, current)...)
			// We need to find the next index
			i++
			open := 1
			for ; ; i++ {
				if s[i] == '(' {
					open++
				} else if s[i] == ')' {
					open--
				}
				if open == 0 {
					break
				}
			}
		case '|':
			rooms = append(rooms, current)
			current = root
			if s[i+1] == ')' {
				// Empty option
				rooms = append(rooms, root)
			}
		case ')':
			return rooms
		}
	}

	return rooms
}

func bfs(root *Room) int {
	distance := make(map[Position]int)
	type State struct {
		room *Room
		dst  int
	}
	q := []State{{room: root}}

	for len(q) != 0 {
		state := q[0]
		q = q[1:]

		if dst, exists := distance[state.room.pos]; exists {
			if dst <= state.dst {
				continue
			}
		}
		distance[state.room.pos] = state.dst

		if state.room.north != nil {
			q = append(q, State{state.room.north, state.dst + 1})
		}
		if state.room.south != nil {
			q = append(q, State{state.room.south, state.dst + 1})
		}
		if state.room.west != nil {
			q = append(q, State{state.room.west, state.dst + 1})
		}
		if state.room.east != nil {
			q = append(q, State{state.room.east, state.dst + 1})
		}
	}

	maxDistance := lib.NewMaxer()
	for _, v := range distance {
		maxDistance.Add(v)
	}
	return maxDistance.Get()
}

func bfs2(root *Room) int {
	distance := make(map[Position]int)
	type State struct {
		room *Room
		dst  int
	}
	q := []State{{room: root}}

	for len(q) != 0 {
		state := q[0]
		q = q[1:]

		if dst, exists := distance[state.room.pos]; exists {
			if dst <= state.dst {
				continue
			}
		}
		distance[state.room.pos] = state.dst

		if state.room.north != nil {
			q = append(q, State{state.room.north, state.dst + 1})
		}
		if state.room.south != nil {
			q = append(q, State{state.room.south, state.dst + 1})
		}
		if state.room.west != nil {
			q = append(q, State{state.room.west, state.dst + 1})
		}
		if state.room.east != nil {
			q = append(q, State{state.room.east, state.dst + 1})
		}
	}

	sum := 0
	for _, v := range distance {
		if v >= 1000 {
			sum++
		}
	}
	return sum
}

type Room struct {
	pos   Position
	north *Room
	south *Room
	west  *Room
	east  *Room
}

type Position struct {
	row int
	col int
}

func (p Position) delta(row, col int) Position {
	p.row += row
	p.col += col
	return p
}

func fs2(input io.Reader) int {
	regex := lib.ReaderToString(input)
	regex = regex[1 : len(regex)-1]

	current := &Room{}

	_ = traverse(regex, 0, current)

	return bfs2(current)
}

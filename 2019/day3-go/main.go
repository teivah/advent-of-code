package main

import (
	"io"
	"strings"

	lib "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	lines := lib.ReaderToStrings(input)

	a := toInstructions(lines[0])
	b := toInstructions(lines[1])

	set := make(map[lib.Position]int)
	cur := lib.Position{}
	for _, ins := range a {
		for i := 0; i < ins.value; i++ {
			switch ins.direction {
			case lib.Left:
				cur = cur.Delta(0, -1)
			case lib.Right:
				cur = cur.Delta(0, 1)
			case lib.Up:
				cur = cur.Delta(1, 0)
			case lib.Down:
				cur = cur.Delta(-1, 0)
			}
			set[cur] = 1
		}
	}

	cur = lib.Position{}
	for _, ins := range b {
		for i := 0; i < ins.value; i++ {
			switch ins.direction {
			case lib.Left:
				cur = cur.Delta(0, -1)
			case lib.Right:
				cur = cur.Delta(0, 1)
			case lib.Up:
				cur = cur.Delta(1, 0)
			case lib.Down:
				cur = cur.Delta(-1, 0)
			}
			if set[cur] == 1 {
				set[cur] = 2
			}
		}
	}

	min := lib.NewMiner()
	for pos, v := range set {
		if v == 2 {
			min.Add(pos.ManhattanZero())
		}
	}

	return min.Get()
}

type Instruction struct {
	direction lib.Direction
	value     int
}

func toInstructions(s string) []Instruction {
	split := strings.Split(s, ",")
	var instructions []Instruction
	for _, v := range split {
		var direction lib.Direction
		switch v[0] {
		case 'R':
			direction = lib.Right
		case 'L':
			direction = lib.Left
		case 'D':
			direction = lib.Down
		case 'U':
			direction = lib.Up
		}
		instructions = append(instructions, Instruction{
			direction: direction,
			value:     lib.StringToInt(v[1:]),
		})
	}
	return instructions
}

func fs2(input io.Reader) int {
	lines := lib.ReaderToStrings(input)

	a := toInstructions(lines[0])
	b := toInstructions(lines[1])

	set := make(map[lib.Position]int)
	cur := lib.Position{}
	steps := 0
	for _, ins := range a {
		for i := 0; i < ins.value; i++ {
			steps++
			switch ins.direction {
			case lib.Left:
				cur = cur.Delta(0, -1)
			case lib.Right:
				cur = cur.Delta(0, 1)
			case lib.Up:
				cur = cur.Delta(1, 0)
			case lib.Down:
				cur = cur.Delta(-1, 0)
			}

			if _, exists := set[cur]; !exists {
				set[cur] = steps
			}
		}
	}

	cur = lib.Position{}
	min := lib.NewMiner()
	steps = 0
	for _, ins := range b {
		for i := 0; i < ins.value; i++ {
			steps++
			switch ins.direction {
			case lib.Left:
				cur = cur.Delta(0, -1)
			case lib.Right:
				cur = cur.Delta(0, 1)
			case lib.Up:
				cur = cur.Delta(1, 0)
			case lib.Down:
				cur = cur.Delta(-1, 0)
			}

			if first, exists := set[cur]; exists {
				min.Add(steps + first)
			}
		}
	}

	return min.Get()
}

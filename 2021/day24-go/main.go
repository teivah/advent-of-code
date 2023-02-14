package main

import (
	"bufio"
	"fmt"
	"io"

	aoc "github.com/teivah/advent-of-code"
)

// Brute force, not working
func fs0(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	var instructions []Apply
	s := &State{}
	for scanner.Scan() {
		line := scanner.Text()
		del := aoc.NewDelimiter(line, " ")
		switch line[:3] {
		case "inp":
			instructions = append(instructions, s.inp(toRegInt(del.GetString(1))))
		case "add":
			instructions = append(instructions, s.add(toRegInt(del.GetString(1)), toRegInt(del.GetString(2))))
		case "mul":
			instructions = append(instructions, s.mul(toRegInt(del.GetString(1)), toRegInt(del.GetString(2))))
		case "div":
			instructions = append(instructions, s.div(toRegInt(del.GetString(1)), toRegInt(del.GetString(2))))
		case "mod":
			instructions = append(instructions, s.mod(toRegInt(del.GetString(1)), toRegInt(del.GetString(2))))
		case "eql":
			instructions = append(instructions, s.eql(toRegInt(del.GetString(1)), toRegInt(del.GetString(2))))
		}
	}

	number := newNumber()
	for {
		s.index = 0
		s.input = number.cur
		s.reg = make(map[string]int)
		for _, ins := range instructions {
			ins()
		}

		if s.reg["z"] == 0 {
			fmt.Println(s.reg)
			return 0
		}

		number.dec()
		fmt.Println(number.cur)
	}
}

type Number struct {
	cur []int
}

func newNumber() *Number {
	return &Number{
		cur: []int{9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9},
	}
}

func (n *Number) dec() {
	v := 13
	for {
		if n.cur[v] == 0 {
			n.cur[v] = 9
			v--
			continue
		}
		n.cur[v]--
		break
	}
}

type Apply func()

type State struct {
	reg   map[string]int
	input []int
	index int
}

func (s *State) inp(a RegInt) Apply {
	return func() {
		s.reg[a.reg] = s.input[s.index]
		s.index++
	}
}

func (s *State) add(a, b RegInt) Apply {
	return func() {
		s.reg[a.reg] = a.getValue(s) + b.getValue(s)
	}
}

func (s *State) mul(a, b RegInt) Apply {
	return func() {
		s.reg[a.reg] = a.getValue(s) * b.getValue(s)
	}
}

func (s *State) div(a, b RegInt) Apply {
	return func() {
		s.reg[a.reg] = a.getValue(s) / b.getValue(s)
	}
}

func (s *State) mod(a, b RegInt) Apply {
	return func() {
		s.reg[a.reg] = a.getValue(s) % b.getValue(s)
	}
}

func (s *State) eql(a, b RegInt) Apply {
	return func() {
		if a.getValue(s) == b.getValue(s) {
			s.reg[a.reg] = 1
		} else {
			s.reg[a.reg] = 0
		}
	}
}

type RegInt struct {
	reg   string
	value int
}

func toRegInt(s string) RegInt {
	v, ok := aoc.TryStringToInt(s)
	if ok {
		return RegInt{value: v}
	}
	return RegInt{reg: s}
}

func (ri RegInt) getValue(s *State) int {
	if ri.reg == "" {
		return ri.value
	}
	return s.reg[ri.reg]
}

func fs1(input io.Reader) int {
	lines := aoc.ReaderToStrings(input)
	return max(extractEquations(lines))
}

func fs2(input io.Reader) int {
	lines := aoc.ReaderToStrings(input)
	return min(extractEquations(lines))
}

type Equation struct {
	lhs Expression
	rhs Expression
}

type Expression struct {
	index int
	value int
}

func max(equations []Equation) int {
	digits := make([]int, 14)
	for _, equation := range equations {
		if equation.rhs.value < 0 {
			digits[equation.lhs.index] = 9 + equation.rhs.value
			digits[equation.rhs.index] = 9
		} else {
			digits[equation.lhs.index] = 9
			digits[equation.rhs.index] = 9 - equation.rhs.value
		}
	}
	result := 0
	for i := 0; i < 14; i++ {
		result = result*10 + digits[i]
	}
	return result
}

func min(equations []Equation) int {
	digits := make([]int, 14)
	for _, equation := range equations {
		if equation.rhs.value < 0 {
			digits[equation.lhs.index] = 1
			digits[equation.rhs.index] = 1 - equation.rhs.value
		} else {
			digits[equation.lhs.index] = 1 + equation.rhs.value
			digits[equation.rhs.index] = 1
		}
	}
	result := 0
	for i := 0; i < 14; i++ {
		result = result*10 + digits[i]
	}
	return result
}

func extractEquations(input []string) []Equation {
	equations := make([]Equation, 7)
	j := 0
	var s []Expression
	for i := 0; i < 14; i++ {
		if input[18*i+4] == "div z 1" {
			value := aoc.NewDelimiter(input[18*i+15], " ").GetInt(2)
			s = append(s, Expression{i, value})
		} else {
			value := aoc.NewDelimiter(input[18*i+5], " ").GetInt(2)
			peek := s[len(s)-1]
			s = s[:len(s)-1]
			equations[j] = Equation{Expression{i, 0}, Expression{peek.index, peek.value + value}}
			j++
		}
	}
	return equations
}

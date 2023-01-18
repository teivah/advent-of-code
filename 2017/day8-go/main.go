package main

import (
	"bufio"
	"fmt"
	lib "github.com/teivah/advent-of-code"
	"io"
	"math"
	"strconv"
)

func fs1(input io.Reader) (int, error) {
	scanner := bufio.NewScanner(input)
	var instructions []Instruction
	reg := make(map[string]int)
	for scanner.Scan() {
		instruction := toInstruction(scanner.Text())
		instructions = append(instructions, instruction)
		reg[instruction.reg] = 0
	}

	for _, ins := range instructions {
		action := 0
		if ins.increment {
			action += ins.value
		} else {
			action -= ins.value
		}

		a := reg[ins.cond.reg]
		b := ins.cond.value
		switch ins.cond.op {
		case less:
			if a < b {
				reg[ins.reg] += action
			}
		case lessOrEqual:
			if a <= b {
				reg[ins.reg] += action
			}
		case greater:
			if a > b {
				reg[ins.reg] += action
			}
		case greaterOrEqual:
			if a >= b {
				reg[ins.reg] += action
			}
		case equal:
			if a == b {
				reg[ins.reg] += action
			}
		case different:
			if a != b {
				reg[ins.reg] += action
			}
		}
	}

	max := math.MinInt
	for _, v := range reg {
		max = lib.Max(max, v)
	}

	fmt.Println(reg)

	return max, nil
}

func toInstruction(s string) Instruction {
	spaces := lib.IndexAll(s, " ")
	reg := s[:spaces[0]]
	increment := true
	if s[spaces[0]+1:spaces[1]] == "dec" {
		increment = false
	}
	value, err := strconv.Atoi(s[spaces[1]+1 : spaces[2]])
	if err != nil {
		panic(err)
	}
	return Instruction{
		reg:       reg,
		increment: increment,
		value:     value,
		cond:      toCondition(s[spaces[3]+1:]),
	}
}

// "t != -1976"
func toCondition(s string) Condition {
	spaces := lib.IndexAll(s, " ")
	reg := s[:spaces[0]]
	var op operator
	switch s[spaces[0]+1 : spaces[1]] {
	case "<":
		op = less
	case "<=":
		op = lessOrEqual
	case ">":
		op = greater
	case ">=":
		op = greaterOrEqual
	case "==":
		op = equal
	case "!=":
		op = different
	}
	value, err := strconv.Atoi(s[spaces[1]+1:])
	if err != nil {
		panic(err)
	}
	return Condition{
		reg:   reg,
		op:    op,
		value: value,
	}
}

type Instruction struct {
	reg       string
	increment bool
	value     int
	cond      Condition
}

type Condition struct {
	reg   string
	op    operator
	value int
}

type operator int

const (
	less operator = iota
	lessOrEqual
	greater
	greaterOrEqual
	equal
	different
)

func fs2(input io.Reader) (int, error) {
	scanner := bufio.NewScanner(input)
	var instructions []Instruction
	reg := make(map[string]int)
	for scanner.Scan() {
		instruction := toInstruction(scanner.Text())
		instructions = append(instructions, instruction)
		reg[instruction.reg] = 0
	}

	max := math.MinInt
	for _, ins := range instructions {
		action := 0
		if ins.increment {
			action += ins.value
		} else {
			action -= ins.value
		}

		a := reg[ins.cond.reg]
		b := ins.cond.value
		switch ins.cond.op {
		case less:
			if a < b {
				reg[ins.reg] += action
			}
		case lessOrEqual:
			if a <= b {
				reg[ins.reg] += action
			}
		case greater:
			if a > b {
				reg[ins.reg] += action
			}
		case greaterOrEqual:
			if a >= b {
				reg[ins.reg] += action
			}
		case equal:
			if a == b {
				reg[ins.reg] += action
			}
		case different:
			if a != b {
				reg[ins.reg] += action
			}
		}
		max = lib.Max(max, reg[ins.reg])
	}

	fmt.Println(reg)

	return max, nil
}

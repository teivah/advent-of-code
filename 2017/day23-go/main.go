package main

import (
	"bufio"
	"fmt"
	"io"
	"strconv"
	"strings"

	lib "github.com/teivah/advent-of-code"
)

type State struct {
	reg                   map[string]int
	offset                int
	mulInstructionInvoked int
}

func (s *State) print() {
	fmt.Println(s.offset, s.reg)
}

type ValueReg struct {
	i *int
	s *string
}

func (vr ValueReg) get(state *State) int {
	if vr.i != nil {
		return *vr.i
	}
	return state.reg[*vr.s]
}

func toValueReg(s string) ValueReg {
	i, err := strconv.Atoi(s)
	if err != nil {
		return ValueReg{s: &s}
	}
	return ValueReg{i: &i}
}

type apply func(state *State)

func set(x ValueReg, y ValueReg) apply {
	return func(state *State) {
		state.offset++
		state.reg[*x.s] = y.get(state)
	}
}

func sub(x ValueReg, y ValueReg) apply {
	return func(state *State) {
		state.offset++
		state.reg[*x.s] -= y.get(state)
	}
}

func mul(x ValueReg, y ValueReg) apply {
	return func(state *State) {
		state.mulInstructionInvoked++
		state.offset++
		state.reg[*x.s] *= y.get(state)
	}
}

func jnz(x ValueReg, y ValueReg) apply {
	return func(state *State) {
		v := x.get(state)
		if v != 0 {
			state.offset += y.get(state)
		} else {
			state.offset++
		}
	}
}

func fs1(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	var cmds []apply
	for scanner.Scan() {
		s := scanner.Text()
		del := lib.NewDelimiter(s, " ")

		if strings.HasPrefix(s, "set") {
			cmds = append(cmds, set(toValueReg(del.GetString(1)), toValueReg(del.GetString(2))))
		} else if strings.HasPrefix(s, "sub") {
			cmds = append(cmds, sub(toValueReg(del.GetString(1)), toValueReg(del.GetString(2))))
		} else if strings.HasPrefix(s, "mul") {
			cmds = append(cmds, mul(toValueReg(del.GetString(1)), toValueReg(del.GetString(2))))
		} else if strings.HasPrefix(s, "jnz") {
			cmds = append(cmds, jnz(toValueReg(del.GetString(1)), toValueReg(del.GetString(2))))
		} else {
			panic(s)
		}
	}

	state := State{
		reg: make(map[string]int),
	}
	for {
		if state.offset >= len(cmds) {
			break
		}
		cmds[state.offset](&state)
	}

	return state.mulInstructionInvoked
}

func fs2() int {
	var (
		b = 109900
		d = 0
		f = 0
		g = 0
		h = 0
	)

	for {
		f = 1
		for d = 2; d*d <= b; d++ {
			if b%d == 0 {
				f = 0
				break
			}
		}

		if f == 0 {
			h++
		}
		g = b - 126900
		b += 17

		if g == 0 {
			return h
		}
	}
}

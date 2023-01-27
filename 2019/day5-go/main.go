package main

import (
	"io"
	"strings"

	lib "github.com/teivah/advent-of-code"
)

func fs(reader io.Reader, input int) int {
	s := lib.ReaderToString(reader)
	codes := lib.StringsToInts(strings.Split(s, ","))

	state := &State{
		codes:  codes,
		offset: 0,
		over:   false,
		mode:   0,
		input:  input,
	}
	state.instructions = map[int]Apply{
		1:  state.opcode1,
		2:  state.opcode2,
		3:  state.opcode3,
		4:  state.opcode4,
		5:  state.opcode5,
		6:  state.opcode6,
		7:  state.opcode7,
		8:  state.opcode8,
		99: state.opcode99,
	}

	for !state.over {
		if state.offset >= len(codes) {
			break
		}
		state.execute()
	}

	return state.output
}

func (s *State) execute() {
	opcode := toOpcode(s.codes[s.offset])
	s.instructions[opcode.opcode](opcode.ctx)
}

type Opcode struct {
	opcode int
	ctx    Context
}

func toOpcode(i int) Opcode {
	var v []int
	for i != 0 {
		v = append(v, i%10)
		i /= 10
	}

	res := make([]int, 0, len(v))
	for idx := 0; idx < 5-len(v); idx++ {
		res = append(res, 0)
	}

	for idx := 0; idx < len(v); idx++ {
		res = append(res, v[len(v)-idx-1])
	}

	return Opcode{
		opcode: res[3]*10 + res[4],
		ctx: Context{
			modes: []int{res[2], res[1], res[0]},
		},
	}
}

type State struct {
	codes        []int
	offset       int
	over         bool
	mode         int
	input        int
	output       int
	instructions map[int]Apply
}

type Context struct {
	modes []int
}

func (s *State) getOutputIndex(param int) int {
	return s.codes[s.offset+param+1]
}

func (s *State) getInput(ctx Context, param int) int {
	if ctx.modes[param] == 0 {
		return s.codes[s.codes[s.offset+1+param]]
	}
	return s.codes[s.offset+1+param]
}

type Apply func(ctx Context)

func (s *State) opcode1(ctx Context) {
	s.codes[s.getOutputIndex(2)] = s.getInput(ctx, 0) + s.getInput(ctx, 1)
	s.offset += 4
}

func (s *State) opcode2(ctx Context) {
	s.codes[s.getOutputIndex(2)] = s.getInput(ctx, 0) * s.getInput(ctx, 1)
	s.offset += 4
}

func (s *State) opcode3(ctx Context) {
	s.codes[s.getOutputIndex(0)] = s.input
	s.offset += 2
}

func (s *State) opcode4(ctx Context) {
	s.output = s.getInput(ctx, 0)
	s.offset += 2
}

func (s *State) opcode5(ctx Context) {
	if s.getInput(ctx, 0) != 0 {
		s.offset = s.getInput(ctx, 1)
	} else {
		s.offset += 3
	}
}

func (s *State) opcode6(ctx Context) {
	if s.getInput(ctx, 0) == 0 {
		s.offset = s.getInput(ctx, 1)
	} else {
		s.offset += 3
	}
}

func (s *State) opcode7(ctx Context) {
	if s.getInput(ctx, 0) < s.getInput(ctx, 1) {
		s.codes[s.getOutputIndex(2)] = 1
	} else {
		s.codes[s.getOutputIndex(2)] = 0
	}
	s.offset += 4
}

func (s *State) opcode8(ctx Context) {
	if s.getInput(ctx, 0) == s.getInput(ctx, 1) {
		s.codes[s.getOutputIndex(2)] = 1
	} else {
		s.codes[s.getOutputIndex(2)] = 0
	}
	s.offset += 4
}

func (s *State) opcode99(ctx Context) {
	s.over = true
}

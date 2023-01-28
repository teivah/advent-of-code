package main

import (
	"io"
	"strings"

	lib "github.com/teivah/advent-of-code"
)

func fs1(reader io.Reader) int {
	s := lib.ReaderToString(reader)
	codes := lib.StringsToInts(strings.Split(s, ","))

	const input = 1
	output, _ := run(codes, input)
	return output
}

func run(codes []int, input int) (int, bool) {
	state := &State{
		program: codes,
		memory:  make([]int, 1000),
		offset:  0,
		over:    false,
		input:   input,
	}
	state.instructions = map[int]Apply{
		1:  state.plus,
		2:  state.mult,
		3:  state.in,
		4:  state.out,
		5:  state.jnz,
		6:  state.jiz,
		7:  state.lt,
		8:  state.eq,
		9:  state.rlt,
		99: state.exit,
	}

	for !state.over {
		if state.offset >= len(codes) {
			break
		}
		state.execute()
	}

	return state.output, state.over
}

func (s *State) execute() {
	opcode := toOpcode(s.program[s.offset])
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
	program      []int
	memory       []int
	offset       int
	over         bool
	output       int
	instructions map[int]Apply
	input        int
	relativeMode int
}

type Context struct {
	modes []int
}

func (s *State) getOutputIndex(param int) int {
	return s.program[s.offset+param+1]
}

func (s *State) getInput(ctx Context, param int) int {
	switch ctx.modes[param] {
	case 0:
		return s.program[s.program[s.offset+1+param]]
	case 1:
		return s.program[s.offset+1+param]
	case 2:
		return s.program[s.program[s.offset+1+param]+s.relativeMode]
	default:
		panic(ctx.modes[param])
	}
}

type Apply func(ctx Context)

func (s *State) plus(ctx Context) {
	s.program[s.getOutputIndex(2)] = s.getInput(ctx, 0) + s.getInput(ctx, 1)
	s.offset += 4
}

func (s *State) mult(ctx Context) {
	s.program[s.getOutputIndex(2)] = s.getInput(ctx, 0) * s.getInput(ctx, 1)
	s.offset += 4
}

func (s *State) in(ctx Context) {
	s.program[s.getOutputIndex(0)] = s.input
	s.offset += 2
}

func (s *State) out(ctx Context) {
	s.output = s.getInput(ctx, 0)
	s.offset += 2
}

func (s *State) jnz(ctx Context) {
	if s.getInput(ctx, 0) != 0 {
		s.offset = s.getInput(ctx, 1)
	} else {
		s.offset += 3
	}
}

func (s *State) jiz(ctx Context) {
	if s.getInput(ctx, 0) == 0 {
		s.offset = s.getInput(ctx, 1)
	} else {
		s.offset += 3
	}
}

func (s *State) lt(ctx Context) {
	if s.getInput(ctx, 0) < s.getInput(ctx, 1) {
		s.program[s.getOutputIndex(2)] = 1
	} else {
		s.program[s.getOutputIndex(2)] = 0
	}
	s.offset += 4
}

func (s *State) eq(ctx Context) {
	if s.getInput(ctx, 0) == s.getInput(ctx, 1) {
		s.program[s.getOutputIndex(2)] = 1
	} else {
		s.program[s.getOutputIndex(2)] = 0
	}
	s.offset += 4
}

func (s *State) rlt(ctx Context) {
	s.relativeMode = s.getInput(ctx, 0)
	s.offset += 2
}

func (s *State) exit(ctx Context) {
	s.over = true
}

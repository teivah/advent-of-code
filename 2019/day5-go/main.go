package main

import (
	"bufio"
	"io"
	"strings"

	lib "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	s := lib.ReaderToString(input)
	codes := lib.StringsToInts(strings.Split(s, ","))

	state := &State{
		codes:  codes,
		offset: 0,
		over:   false,
		mode:   0,
		input:  1,
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
	var apply Apply
	switch opcode.opcode {
	case 1:
		apply = s.opcode1
	case 2:
		apply = s.opcode2
	case 3:
		apply = s.opcode3
	case 4:
		apply = s.opcode4
	case 99:
		apply = s.opcode99
	default:
		panic(opcode.opcode)
	}
	apply(opcode.ctx)
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
	codes  []int
	offset int
	over   bool
	mode   int
	input  int
	output int
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

func (s *State) opcode99(ctx Context) {
	s.over = true
}

func fs2(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
		line := scanner.Text()
		_ = line
	}

	return 42
}

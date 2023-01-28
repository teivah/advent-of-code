package main

import (
	"fmt"
	"io"
	"strings"

	lib "github.com/teivah/advent-of-code"
)

func fs1(reader io.Reader) []int {
	s := lib.ReaderToString(reader)
	codes := lib.StringsToInts(strings.Split(s, ","))

	const input = 1
	output, _ := run(codes, input)
	return output
}

func run(codes []int, input int) ([]int, bool) {
	state := &State{
		memory: append(codes, make([]int, 100)...),
		offset: 0,
		over:   false,
		input:  input,
		debug:  true,
	}
	state.functions = map[int]Apply{
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
	opcode := toOpcode(s.memory[s.offset])
	s.functions[opcode.opcode](opcode.ctx)
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
	debug        bool
	memory       []int
	offset       int
	over         bool
	output       []int
	functions    map[int]Apply
	input        int
	relativeBase int
}

type Context struct {
	modes []int
}

func (c Context) String() string {
	var s []string
	for _, mode := range c.modes {
		switch mode {
		case 0:
			s = append(s, "position")
		case 1:
			s = append(s, "immediate")
		case 2:
			s = append(s, "relative")
		default:
			panic(mode)
		}
	}
	return fmt.Sprintf("[%s]", strings.Join(s, ", "))
}

func (c Context) Until(i int) string {
	var s []string
	for _, mode := range c.modes[:i] {
		switch mode {
		case 0:
			s = append(s, "position")
		case 1:
			s = append(s, "immediate")
		case 2:
			s = append(s, "relative")
		default:
			panic(mode)
		}
	}
	return fmt.Sprintf("[%s]", strings.Join(s, ", "))
}

type Apply func(ctx Context)

func (s *State) program(ctx Context, parameter int) int {
	instructionIndex := s.offset + 1 + parameter
	instructionValue := s.memory[instructionIndex]

	switch ctx.modes[parameter] {
	case 0: // Position
		return s.memory[instructionValue]
	case 1: // Immediate
		return instructionValue
	case 2: // Relative
		offset := instructionValue + s.relativeBase
		return s.memory[offset]
	default:
		panic(ctx.modes[parameter])
	}
}

func (s *State) get(ctx Context, parameter int) int {
	instructionIndex := s.offset + 1 + parameter
	instructionValue := s.memory[instructionIndex]

	switch ctx.modes[parameter] {
	case 0: // Position
		return s.memory[instructionValue]
	case 1: // Immediate
		return instructionValue
	case 2: // Relative
		return s.memory[instructionValue+s.relativeBase]
	default:
		panic(ctx.modes[parameter])
	}
}

func (s *State) index(ctx Context, parameter int) int {
	instructionIndex := s.offset + 1 + parameter
	instructionValue := s.memory[instructionIndex]

	switch ctx.modes[parameter] {
	case 0: // Position
		return instructionValue
	case 1: // Immediate
		panic(ctx.modes[parameter])
	case 2: // Relative
		return instructionValue + s.relativeBase
	default:
		panic(ctx.modes[parameter])
	}
}

func (s *State) plus(ctx Context) {
	if s.debug {
		fmt.Println("plus", ctx, s.memory[s.offset+1], s.memory[s.offset+2], s.memory[s.offset+3])
	}

	s.memory[s.index(ctx, 2)] = s.get(ctx, 0) + s.get(ctx, 1)
	s.offset += 4
}

func (s *State) mult(ctx Context) {
	if s.debug {
		fmt.Println("mult", ctx, s.memory[s.offset+1], s.memory[s.offset+2], s.memory[s.offset+3])
	}

	s.memory[s.index(ctx, 2)] = s.get(ctx, 0) * s.get(ctx, 1)
	s.offset += 4
}

func (s *State) in(ctx Context) {
	if s.debug {
		fmt.Println("in", ctx.Until(1), s.memory[s.offset+1])
	}

	s.memory[s.index(ctx, 0)] = s.input
	s.offset += 2
}

func (s *State) out(ctx Context) {
	if s.debug {
		fmt.Println("out", ctx.Until(1), s.memory[s.offset+1])
	}

	s.output = append(s.output, s.get(ctx, 0))
	s.offset += 2
}

func (s *State) jnz(ctx Context) {
	if s.debug {
		fmt.Println("jnz", ctx.Until(2), s.memory[s.offset+1], s.memory[s.offset+2])
	}

	if s.get(ctx, 0) != 0 {
		s.offset = s.get(ctx, 1)
	} else {
		s.offset += 3
	}
}

func (s *State) jiz(ctx Context) {
	if s.debug {
		fmt.Println("jiz", ctx.Until(2), s.memory[s.offset+1], s.memory[s.offset+2])
	}

	if s.get(ctx, 0) == 0 {
		s.offset = s.get(ctx, 1)
	} else {
		s.offset += 3
	}
}

func (s *State) lt(ctx Context) {
	if s.debug {
		fmt.Println("lt", ctx, s.memory[s.offset+1], s.memory[s.offset+2], s.memory[s.offset+3])
	}

	if s.get(ctx, 0) < s.get(ctx, 1) {
		s.memory[s.index(ctx, 2)] = 1
	} else {
		s.memory[s.index(ctx, 2)] = 0
	}
	s.offset += 4
}

func (s *State) eq(ctx Context) {
	if s.debug {
		fmt.Println("eq", ctx, s.memory[s.offset+1], s.memory[s.offset+2], s.memory[s.offset+3])
	}

	if s.get(ctx, 0) == s.get(ctx, 1) {
		s.memory[s.index(ctx, 2)] = 1
	} else {
		s.memory[s.index(ctx, 2)] = 0
	}
	s.offset += 4
}

func (s *State) rlt(ctx Context) {
	if s.debug {
		fmt.Println("rlt", ctx.Until(1), s.memory[s.offset+1])
	}

	s.relativeBase += s.program(ctx, 0)
	s.offset += 2
}

func (s *State) exit(ctx Context) {
	s.over = true
}

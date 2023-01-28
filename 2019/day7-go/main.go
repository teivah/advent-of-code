package main

import (
	"io"
	"strings"

	lib "github.com/teivah/advent-of-code"
)

func fs1(reader io.Reader) int {
	s := lib.ReaderToString(reader)
	codes := lib.StringsToInts(strings.Split(s, ","))
	perms := permutations(0, []int{0, 1, 2, 3, 4})

	max := lib.NewMaxer()
	for _, perm := range perms {
		input := 0
		cpy := lib.CopyInts(codes)
		for i := 0; i < 5; i++ {
			input, _ = run(cpy, input, perm[i])
		}
		max.Add(input)
	}
	return max.Get()
}

func run(codes []int, input int, phaseSetting int) (int, bool) {
	state := &State{
		codes:  codes,
		offset: 0,
		over:   false,

		phaseSetting: phaseSetting,
		input:        input,
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

func runFromState(state *State) (int, bool) {
	for !state.over && !state.paused {
		if state.offset >= len(state.codes) {
			break
		}
		state.execute()
	}

	return state.output, state.over
}

func permutations(idx int, v []int) [][]int {
	if idx == len(v) {
		res := make([]int, len(v))
		copy(res, v)
		return [][]int{res}
	}

	var res [][]int
	for i := idx; i < len(v); i++ {
		v[i], v[idx] = v[idx], v[i]
		res = append(res, permutations(idx+1, v)...)
		v[i], v[idx] = v[idx], v[i]
	}
	return res
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
	paused       bool
	output       int
	instructions map[int]Apply

	phaseSettingGiven bool
	phaseSetting      int
	input             int
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

func (s *State) plus(ctx Context) {
	s.codes[s.getOutputIndex(2)] = s.getInput(ctx, 0) + s.getInput(ctx, 1)
	s.offset += 4
}

func (s *State) mult(ctx Context) {
	s.codes[s.getOutputIndex(2)] = s.getInput(ctx, 0) * s.getInput(ctx, 1)
	s.offset += 4
}

func (s *State) in(ctx Context) {
	// TODO Disable for the next day
	//s.codes[s.getOutputIndex(0)] = s.input
	if !s.phaseSettingGiven {
		s.codes[s.getOutputIndex(0)] = s.phaseSetting
		s.phaseSettingGiven = true
	} else {
		s.codes[s.getOutputIndex(0)] = s.input
	}
	s.offset += 2
}

func (s *State) out(ctx Context) {
	s.output = s.getInput(ctx, 0)
	s.offset += 2
	s.paused = true
	// TODO Disable for the next day
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
		s.codes[s.getOutputIndex(2)] = 1
	} else {
		s.codes[s.getOutputIndex(2)] = 0
	}
	s.offset += 4
}

func (s *State) eq(ctx Context) {
	if s.getInput(ctx, 0) == s.getInput(ctx, 1) {
		s.codes[s.getOutputIndex(2)] = 1
	} else {
		s.codes[s.getOutputIndex(2)] = 0
	}
	s.offset += 4
}

func (s *State) exit(ctx Context) {
	s.over = true
}

// Provide each amplifier its phase setting at its first input instruction?

func fs2(reader io.Reader) int {
	s := lib.ReaderToString(reader)
	codes := lib.StringsToInts(strings.Split(s, ","))
	perms := permutations(0, []int{5, 6, 7, 8, 9})

	max := lib.NewMaxer()

	for _, perm := range perms {
		input := 0
		states := make([]*State, 0, 5)
		for i := 0; i < 5; i++ {
			state := &State{
				codes:  lib.CopyInts(codes),
				offset: 0,
				over:   false,
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
				99: state.exit,
			}
			states = append(states, state)
		}

	outer:
		for {
			for i := 0; i < 5; i++ {
				states[i].over = false
				states[i].paused = false
				states[i].phaseSetting = perm[i]
				states[i].input = input
				v, over := runFromState(states[i])
				input = v
				if i == 4 && over {
					break outer
				}
			}
		}
		max.Add(input)
	}
	return max.Get()
}

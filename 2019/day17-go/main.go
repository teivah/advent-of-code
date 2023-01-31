package main

import (
	"fmt"
	"io"
	"strings"

	lib "github.com/teivah/advent-of-code"
)

func fs1(reader io.Reader) int {
	s := lib.ReaderToString(reader)
	codes := lib.StringsToInts(strings.Split(s, ","))

	state := run(codes, nil)
	return state.calculateAlignments()
}

func run(codes []int, postActions func(*State)) *State {
	state := &State{
		memory: append(codes, make([]int, 10000)...),
		offset: 0,
		over:   false,
		//debug:  true,

		grid: make(map[lib.Position]UnitType),
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

	if postActions != nil {
		postActions(state)
	}

	for !state.over {
		if state.offset >= len(codes) {
			break
		}
		state.execute()
	}

	return state
}

func (s *State) calculateAlignments() int {
	maxRow := lib.NewMaxer()
	maxCol := lib.NewMaxer()

	for position := range s.grid {
		maxRow.Add(position.Row)
		maxCol.Add(position.Col)
	}

	sum := 0
	intersection := make(map[lib.Position]bool)
	for position, t := range s.grid {
		if t == empty {
			continue
		}

		if position.Row == 0 || position.Col == 0 || position.Row == maxRow.Get() || position.Col == maxCol.Get() {
			continue
		}

		if s.grid[position.Delta(-1, 0)] == scaffold &&
			s.grid[position.Delta(1, 0)] == scaffold &&
			s.grid[position.Delta(0, -1)] == scaffold &&
			s.grid[position.Delta(0, 1)] == scaffold {
			intersection[position] = true
			sum += position.Row * position.Col
		}
	}

	return sum
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
	functions    map[int]Apply
	relativeBase int

	grid         map[lib.Position]UnitType
	gridPosition lib.Position
	position     lib.Position
	direction    lib.Direction
	result       []int
	iResult      int
	collected    int
}

type UnitType int

const (
	scaffold UnitType = iota
	empty
)

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

func (s *State) set(ctx Context, parameter, value int) {
	s.memory[s.index(ctx, parameter)] = value
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

	s.set(ctx, 2, s.get(ctx, 0)+s.get(ctx, 1))
	s.offset += 4
}

func (s *State) mult(ctx Context) {
	if s.debug {
		fmt.Println("mult", ctx, s.memory[s.offset+1], s.memory[s.offset+2], s.memory[s.offset+3])
	}

	s.set(ctx, 2, s.get(ctx, 0)*s.get(ctx, 1))
	s.offset += 4
}

func (s *State) in(ctx Context) {
	if s.debug {
		fmt.Println("in", ctx.Until(1), s.memory[s.offset+1])
	}

	s.set(ctx, 0, s.result[s.iResult])
	s.iResult++
	s.offset += 2
}

func toASCII(s string) []int {
	res := make([]int, len(s))
	for i := 0; i < len(s); i++ {
		res[i] = int(rune(s[i]))
	}
	return res
}

func (s *State) out(ctx Context) {
	if s.debug {
		fmt.Println("out", ctx.Until(1), s.memory[s.offset+1])
	}

	v := s.get(ctx, 0)

	switch v {
	case 35:
		s.grid[s.gridPosition] = scaffold
		s.gridPosition = s.gridPosition.Delta(0, 1)
	case 46:
		s.grid[s.gridPosition] = empty
		s.gridPosition = s.gridPosition.Delta(0, 1)
	case 10:
		s.gridPosition = s.gridPosition.Delta(1, 0)
		s.gridPosition.Col = 0
	default:
		switch v {
		case '^':
			s.direction = lib.Up
		case 'v':
			s.direction = lib.Down
		case '<':
			s.direction = lib.Left
		case '>':
			s.direction = lib.Right
		}
		s.position = s.gridPosition
	}

	fmt.Print(string([]rune{rune(v)}))
	s.offset += 2
	s.collected = v
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
		s.set(ctx, 2, 1)
	} else {
		s.set(ctx, 2, 0)
	}
	s.offset += 4
}

func (s *State) eq(ctx Context) {
	if s.debug {
		fmt.Println("eq", ctx, s.memory[s.offset+1], s.memory[s.offset+2], s.memory[s.offset+3])
	}

	if s.get(ctx, 0) == s.get(ctx, 1) {
		s.set(ctx, 2, 1)
	} else {
		s.set(ctx, 2, 0)
	}
	s.offset += 4
}

func (s *State) rlt(ctx Context) {
	if s.debug {
		fmt.Println("rlt", ctx.Until(1), s.memory[s.offset+1])
	}

	s.relativeBase += s.get(ctx, 0)
	s.offset += 2
}

func (s *State) exit(ctx Context) {
	s.over = true
}

func fs2(reader io.Reader) int {
	s := lib.ReaderToString(reader)
	codes := lib.StringsToInts(strings.Split(s, ","))

	state := run(codes, func(state *State) {
		state.memory[0] = 2

		state.result = toASCII(`A,B,A,C,B,C,A,C,B,C
L,8,R,10,L,10
R,10,L,8,L,8,L,10
L,4,L,6,L,8,L,8
y
`)
	})
	return state.collected
}

package main

import (
	"fmt"
	"io"
	"math"
	"strings"

	lib "github.com/teivah/advent-of-code"
)

const (
	empty  = 0
	wall   = 1
	block  = 2
	paddle = 3
	ball   = 4
)

func fs1(reader io.Reader) int {
	codes := lib.StringsToInts(strings.Split(lib.ReaderToString(reader), ","))

	s := run(codes, nil)

	minX := math.MaxInt
	minY := math.MaxInt
	maxX := math.MinInt
	maxY := math.MinInt
	for _, x := range s.x {
		minX = lib.Min(minX, x)
		maxX = lib.Max(maxX, x)
	}
	for _, y := range s.y {
		minY = lib.Min(minY, y)
		maxY = lib.Max(maxY, y)
	}

	board := make(map[lib.Position]int)
	for i := 0; i < len(s.x); i++ {
		x := s.x[i]
		y := s.y[i]
		tile := s.tiles[i]
		board[lib.Position{y, x}] = tile
	}

	sum := 0
	for _, v := range board {
		if v == block {
			sum++
		}
	}

	return sum
}

func run(codes []int, preAction func(*State)) *State {
	state := &State{
		memory: append(codes, make([]int, 1000)...),
		offset: 0,
		over:   false,
		board:  make(map[lib.Position]int),
		//debug:  true,
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

	if preAction != nil {
		preAction(state)
	}

	for !state.over {
		if state.offset >= len(codes) {
			break
		}
		state.execute()
	}

	return state
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

	output      int
	x           []int
	y           []int
	tiles       []int
	minX        int
	minY        int
	maxX        int
	maxY        int
	board       map[lib.Position]int
	latestScore int
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
	s.printGame()

	s.set(ctx, 0, 0)
	s.offset += 2
}

func (s *State) out(ctx Context) {
	if s.debug {
		fmt.Println("out", ctx.Until(1), s.memory[s.offset+1])
	}

	v := s.get(ctx, 0)
	switch s.output % 3 {
	case 0:
		s.x = append(s.x, v)
	case 1:
		s.y = append(s.y, v)
	case 2:
		if s.x[len(s.x)-1] == 1 && s.y[len(s.y)-1] == 0 {
			s.latestScore = v
			s.x = s.x[:len(s.x)-1]
			s.y = s.y[:len(s.y)-1]
		} else {
			x := s.x[len(s.x)-1]
			y := s.y[len(s.y)-1]
			s.minX = lib.Min(s.minX, x)
			s.maxX = lib.Max(s.maxX, x)
			s.minY = lib.Min(s.minY, y)
			s.maxY = lib.Max(s.maxY, y)
			s.board[lib.Position{y, x}] = v
			s.tiles = append(s.tiles, v)
		}
	}

	s.output++
	s.offset += 2
}

func (s *State) printGame() {
	fmt.Printf("Score: %v\n", s.latestScore)
	for row := s.minY; row <= s.maxY; row++ {
		for col := s.minX; col <= s.maxX; col++ {
			if v, exists := s.board[lib.Position{row, col}]; exists {
				switch v {
				case empty:
					fmt.Print(" ")
				case wall:
					fmt.Print("|")
				case block:
					fmt.Print("#")
				case paddle:
					fmt.Print("_")
				case ball:
					fmt.Print("O")
				}
			} else {
				fmt.Print(" ")
			}
		}
		fmt.Println()
	}
	fmt.Println()
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
	codes := lib.StringsToInts(strings.Split(lib.ReaderToString(reader), ","))

	run(codes, func(state *State) {
		state.memory[0] = 2
	})

	return 0
}

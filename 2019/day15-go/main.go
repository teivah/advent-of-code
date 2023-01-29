package main

import (
	"fmt"
	"io"
	"strings"

	lib "github.com/teivah/advent-of-code"
)

const (
	inputNorth = 1
	inputSouth = 2
	inputWest  = 3
	inputEast  = 4

	outputHitWall            = 0
	outputMovedOneStep       = 1
	outputMovedOneStepOxygen = 2
)

func fs1(reader io.Reader) int {
	s := lib.ReaderToString(reader)
	codes := lib.StringsToInts(strings.Split(s, ","))

	state := run(codes)
	state.printGrid()

	return state.bfs()
}

func (s *State) bfs() int {
	type entry struct {
		pos lib.Position
		cur int
	}
	q := []entry{{}}
	visited := make(map[lib.Position]bool)

	for len(q) != 0 {
		e := q[0]
		q = q[1:]

		if e.pos == s.oxygen {
			return e.cur
		}

		if visited[e.pos] {
			continue
		}
		visited[e.pos] = true

		if s.grid[e.pos] == unitWall {
			continue
		}

		q = append(q, entry{e.pos.Delta(-1, 0), e.cur + 1})
		q = append(q, entry{e.pos.Delta(1, 0), e.cur + 1})
		q = append(q, entry{e.pos.Delta(0, -1), e.cur + 1})
		q = append(q, entry{e.pos.Delta(0, 1), e.cur + 1})
	}

	return -1
}

func run(codes []int) *State {
	state := &State{
		memory: append(codes, make([]int, 1000)...),
		offset: 0,
		over:   false,
		//debug:  true,

		grid: make(map[lib.Position]Unit),
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
	state.grid[lib.Position{}] = unitEmpty

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

	grid   map[lib.Position]Unit
	stack  []lib.Direction
	pos    lib.Position
	oxygen lib.Position
}

type Unit int

const (
	unitWall Unit = iota + 1
	unitEmpty
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

	var next lib.Direction
	var nextPosition lib.Position
	rollback := false

	defer func() {
		switch next {
		case lib.Up:
			s.set(ctx, 0, inputNorth)
		case lib.Down:
			s.set(ctx, 0, inputSouth)
		case lib.Left:
			s.set(ctx, 0, inputWest)
		case lib.Right:
			s.set(ctx, 0, inputEast)
		}
		s.offset += 2
		s.pos = nextPosition
		if !rollback {
			s.stack = append(s.stack, next)
		}
	}()

	up := s.pos.Delta(-1, 0)
	_, exists := s.grid[up]
	if !exists {
		next = lib.Up
		nextPosition = up
		return
	}

	left := s.pos.Delta(0, -1)
	_, exists = s.grid[left]
	if !exists {
		next = lib.Left
		nextPosition = left
		return
	}

	right := s.pos.Delta(0, 1)
	_, exists = s.grid[right]
	if !exists {
		next = lib.Right
		nextPosition = right
		return
	}

	down := s.pos.Delta(1, 0)
	_, exists = s.grid[down]
	if !exists {
		next = lib.Down
		nextPosition = down
		return
	}

	zero := lib.Position{}
	if s.pos == zero {
		s.over = true
		return
	}

	rollback = true
	last := s.stack[len(s.stack)-1]
	s.stack = s.stack[:len(s.stack)-1]
	next = last.Opposite()
	nextPosition = s.pos.Move(next, 1)
}

func (s *State) printGrid() {
	minRow := lib.NewMiner()
	minCol := lib.NewMiner()
	maxRow := lib.NewMaxer()
	maxCol := lib.NewMaxer()
	for pos := range s.grid {
		minRow.Add(pos.Row)
		maxRow.Add(pos.Row)
		minCol.Add(pos.Col)
		maxCol.Add(pos.Col)
	}

	for row := minRow.Get(); row <= maxRow.Get(); row++ {
		fmt.Printf("%4d ", row)
		for col := minCol.Get(); col <= maxCol.Get(); col++ {
			if row == 0 && col == 0 {
				fmt.Print("X")
				continue
			}
			if row == s.oxygen.Row && col == s.oxygen.Col {
				fmt.Print("X")
				continue
			}

			switch s.grid[lib.Position{row, col}] {
			case unitEmpty:
				fmt.Print(" ")
			case unitWall:
				fmt.Print("#")
			default:
				fmt.Print("?")
			}
		}
		fmt.Println()
	}
	fmt.Println()
}

func (s *State) out(ctx Context) {
	if s.debug {
		fmt.Println("out", ctx.Until(1), s.memory[s.offset+1])
	}

	//s.printGrid()

	v := s.get(ctx, 0)
	switch v {
	case outputHitWall:
		s.grid[s.pos] = unitWall
		last := s.stack[len(s.stack)-1]
		s.stack = s.stack[:len(s.stack)-1]
		s.pos = s.pos.Move(last.Opposite(), 1)
	case outputMovedOneStep:
		s.grid[s.pos] = unitEmpty
	case outputMovedOneStepOxygen:
		s.oxygen = s.pos
		s.grid[s.pos] = unitEmpty
	}

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

	state := run(codes)

	sum := state.fill()
	state.printGrid()

	return sum
}

func (s *State) fill() int {
	type entry struct {
		pos lib.Position
		cur int
	}
	q := []entry{{pos: s.oxygen, cur: 0}}
	visited := make(map[lib.Position]bool)
	latest := 0

	for len(q) != 0 {
		e := q[0]
		q = q[1:]
		latest = e.cur

		if visited[e.pos] {
			continue
		}
		visited[e.pos] = true

		if s.grid[e.pos] == unitWall {
			continue
		}

		s.printGrid()

		q = append(q, entry{e.pos.Delta(-1, 0), e.cur + 1})
		q = append(q, entry{e.pos.Delta(1, 0), e.cur + 1})
		q = append(q, entry{e.pos.Delta(0, -1), e.cur + 1})
		q = append(q, entry{e.pos.Delta(0, 1), e.cur + 1})
	}

	return latest - 1
}

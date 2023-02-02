package main

import (
	"fmt"
	"io"
	"math/rand"
	"os"
	"strings"
	"time"

	lib "github.com/teivah/advent-of-code"
)

func fs(reader io.Reader) int {
	s := lib.ReaderToString(reader)
	codes := lib.StringsToInts(strings.Split(s, ","))

	run(codes, func(state *State) {
		state.messages = make(map[string]struct{})
		state.excluded = map[string]struct{}{
			"infinite loop":       {},
			"escape pod":          {},
			"molten lava":         {},
			"photons":             {},
			"giant electromagnet": {},
		}
	})
	return 1
}

func run(codes []int, postActions func(*State)) *State {
	state := &State{
		memory: append(codes, make([]int, 10000)...),
		offset: 0,
		over:   false,
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

	buffer        []int
	iBuffer       int
	output        string
	excluded      map[string]struct{}
	oldOutput     Output
	messages      map[string]struct{}
	items         []string
	last          bool
	combinations  [][]string
	iCombinations int
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

func (s *State) write(out string) {
	s.buffer = append(s.buffer, toASCII(out+"\n")...)
}

func (s *State) in(ctx Context) {
	if s.debug {
		fmt.Println("in", ctx.Until(1), s.memory[s.offset+1])
	}

	if s.last {
		if s.iBuffer == len(s.buffer) {
			if s.iCombinations == len(s.combinations) {
				panic("")
			}

			s.iBuffer = 0
			s.write("take hologram")
			s.write("take semiconductor")
			s.write("take prime")
			s.write("take monolith")
			s.write("take mutex")
			s.write("take polygon")
			s.write("take weather")
			s.write("take jam")

			next := s.combinations[s.iCombinations]
			for _, item := range next {
				s.write("drop " + item)
			}

			s.write("north")
			s.iCombinations++
		}

		v := s.buffer[s.iBuffer]
		fmt.Printf("%c", v)
		s.set(ctx, 0, v)
		s.iBuffer++

		s.offset += 2
		return
	}

	if s.iBuffer == len(s.buffer) {
		s.buffer = nil
		s.iBuffer = 0
		output := parse(s.output)

		if strings.Contains(output.message, "In the next room") && len(s.items) == 8 {
			s.last = true
			s.combinations = allCombinations(s.items)
			return
		}

		for _, item := range output.items {
			if _, exists := s.excluded[item]; exists {
				continue
			}
			s.write("take " + item)
			s.items = append(s.items, item)
			fmt.Println("take", item)
		}
		s.write(output.randomDoor())
		s.oldOutput = output
		s.output = ""
	}

	v := s.buffer[s.iBuffer]
	fmt.Printf("%c", v)
	s.set(ctx, 0, v)
	s.iBuffer++

	s.offset += 2
}

func allCombinations(elements []string) [][]string {
	var res [][]string

	for i := 0; i < len(elements); i++ {
		res = append(res, combinations(elements, i)...)
	}
	return res
}

func combinations(elements []string, k int) [][]string {
	if k == 0 {
		return [][]string{{}}
	}

	var result [][]string
	for i := 0; i <= len(elements)-k; i++ {
		combinations := combinations(elements[i+1:], k-1)
		for _, combination := range combinations {
			result = append(result, append([]string{elements[i]}, combination...))
		}
	}
	return result
}

func (s *State) out(ctx Context) {
	if s.debug {
		fmt.Println("out", ctx.Until(1), s.memory[s.offset+1])
	}

	v := s.get(ctx, 0)

	fmt.Print(string(rune(v)))
	s.output += string(rune(v))
	s.offset += 2
}

type Output struct {
	message string
	doors   []string
	items   []string
}

func (o Output) randomDoor() string {
	rand.Seed(time.Now().UnixNano())
	n := rand.Intn(len(o.doors))
	return o.doors[n]
}

func parse(s string) Output {
	if strings.Contains(s, "Unrecognized command.") {
		os.Exit(1)
	}

	v := strings.Index(s, "A loud, robotic voice says \"Alert!")
	if v != -1 {
		s = s[v:]
	}

	lines := strings.Split(s, "\n")
	output := Output{}
	for i := 0; i < len(lines); i++ {
		line := lines[i]
		if len(line) == 0 {
			continue
		}
		if strings.HasPrefix(line, "==") {
			output.message = lines[i+1]
			i++
		} else if strings.HasPrefix(line, "Doors here") {
			i++
			for {
				line := lines[i]
				if strings.HasPrefix(line, "-") {
					output.doors = append(output.doors, line[2:])
				} else {
					break
				}
				i++
			}
		} else if strings.HasPrefix(line, "Items here") {
			i++
			for {
				line := lines[i]
				if strings.HasPrefix(line, "-") {
					output.items = append(output.items, line[2:])
				} else {
					break
				}
				i++
			}
		}
	}
	return output
}

func lines(cmds ...string) []int {
	var res []int
	for _, cmd := range cmds {
		res = append(res, toASCII(cmd+"\n")...)
	}
	return res
}

func toASCII(s string) []int {
	res := make([]int, len(s))
	for i := 0; i < len(s); i++ {
		res[i] = int(rune(s[i]))
	}
	return res
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

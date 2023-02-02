package main

import (
	"fmt"
	"io"
	"strings"
	"sync"
	"time"

	lib "github.com/teivah/advent-of-code"
)

const (
	machines = 50
	buffer   = 1000
)

func fs1(reader io.Reader) int {
	s := lib.ReaderToString(reader)
	codes := lib.StringsToInts(strings.Split(s, ","))

	nic := NewNIC()

	for i := 0; i < machines; i++ {
		i := i
		go func() {
			_ = run(codes, func(state *State) {
				state.nic = nic
				state.id = i
			})
		}()
	}

	return (<-nic.result).y
}

type NIC struct {
	chs    []chan int
	over   chan struct{}
	result chan Packet

	mu   sync.RWMutex
	idle []bool
}

func NewNIC() *NIC {
	var chs []chan int
	for i := 0; i < machines; i++ {
		chs = append(chs, make(chan int, buffer))
	}

	return &NIC{
		chs:    chs,
		over:   make(chan struct{}),
		result: make(chan Packet, buffer),
		idle:   make([]bool, machines),
	}
}

func (n *NIC) setIdle(id int, value bool) {
	n.mu.Lock()
	defer n.mu.Unlock()
	n.idle[id] = value
}

func (n *NIC) allIdle() bool {
	n.mu.RLock()
	defer n.mu.RUnlock()
	fmt.Println(n.idle)
	for _, idle := range n.idle {
		if !idle {
			return false
		}
	}
	return true
}

func run(codes []int, postActions func(*State)) *State {
	state := &State{
		memory: append(codes, make([]int, 1000)...),
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

	id           int
	nic          *NIC
	idGiven      bool
	inputBuffer  []int
	outputBuffer []int
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
		fmt.Println("in", s.id, ctx.Until(1), s.memory[s.offset+1])
	}

	if !s.idGiven {
		s.set(ctx, 0, s.id)
		s.idGiven = true
	} else {
		select {
		case v := <-s.nic.chs[s.id]:
			s.set(ctx, 0, v)
		case <-time.After(1 * time.Second):
			s.set(ctx, 0, -1)
			s.nic.setIdle(s.id, true)
		case <-s.nic.over:
			s.over = true
			return
		}
	}

	s.offset += 2
}

func (s *State) out(ctx Context) {
	if s.debug {
		fmt.Println("out", s.id, ctx.Until(1), s.memory[s.offset+1])
	}

	v := s.get(ctx, 0)
	s.nic.setIdle(s.id, false)
	s.outputBuffer = append(s.outputBuffer, v)

	if len(s.outputBuffer) == 3 {
		destination := s.outputBuffer[0]
		if destination == 255 {
			s.nic.result <- Packet{
				x: s.outputBuffer[1],
				y: s.outputBuffer[2],
			}
		} else {
			s.nic.chs[destination] <- s.outputBuffer[1]
			s.nic.chs[destination] <- s.outputBuffer[2]
			s.outputBuffer = nil
		}

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

	nic := NewNIC()

	for i := 0; i < machines; i++ {
		i := i
		go func() {
			_ = run(codes, func(state *State) {
				state.nic = nic
				state.id = i
			})
		}()
	}

	// NAT
	result := make(chan int)
	go func() {
		duration := 3 * time.Second
		timer := time.NewTimer(duration)
		var lastValue Packet
		for {
			timer.Reset(duration)
			select {
			case <-timer.C:
				fmt.Println(len(nic.result))

				//if nic.allIdle() {
				var last Packet

			outer:
				for {
					select {
					case v := <-nic.result:
						last = v
					default:
						break outer
					}
				}

				if last == lastValue {
					result <- last.x
					result <- last.y
					close(result)
					return
				}

				fmt.Println("sending", last)
				nic.chs[0] <- last.y
				lastValue = last
				//} else {
				//	fmt.Println("no")
				//}
			}
		}
	}()

	return <-result
}

type Packet struct {
	x int
	y int
}

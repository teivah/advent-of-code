package main

import (
	"bufio"
	"fmt"
	"io"
	"strconv"
	"strings"
	"sync"
	"time"

	lib "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	var cmds []apply
	for scanner.Scan() {
		s := scanner.Text()
		del := lib.NewDelimiter(s, " ")

		if strings.HasPrefix(s, "snd") {
			cmds = append(cmds, snd(toValueReg(del.GetString(1))))
		} else if strings.HasPrefix(s, "set") {
			cmds = append(cmds, set(toValueReg(del.GetString(1)), toValueReg(del.GetString(2))))
		} else if strings.HasPrefix(s, "add") {
			cmds = append(cmds, add(toValueReg(del.GetString(1)), toValueReg(del.GetString(2))))
		} else if strings.HasPrefix(s, "mul") {
			cmds = append(cmds, mul(toValueReg(del.GetString(1)), toValueReg(del.GetString(2))))
		} else if strings.HasPrefix(s, "mod") {
			cmds = append(cmds, mod(toValueReg(del.GetString(1)), toValueReg(del.GetString(2))))
		} else if strings.HasPrefix(s, "rcv") {
			cmds = append(cmds, rcv(toValueReg(del.GetString(1))))
		} else if strings.HasPrefix(s, "jgz") {
			cmds = append(cmds, jgz(toValueReg(del.GetString(1)), toValueReg(del.GetString(2))))
		} else {
			panic(s)
		}
	}

	offset := 0
	state := State{
		reg: make(map[string]int),
	}
	for {
		cmds[offset](&state)

		if state.recovered {
			return state.frequency
		}

		offset = state.offset
	}
}

type State struct {
	reg       map[string]int
	offset    int
	frequency int

	// Part 1
	recovered bool

	// Part 2
	id       int
	sent     int
	input    <-chan int
	output   chan<- int
	deadlock bool
}

func (s *State) print() {
	fmt.Println(s.offset, s.reg)
}

type ValueReg struct {
	i *int
	s *string
}

func (vr ValueReg) get(state *State) int {
	if vr.i != nil {
		return *vr.i
	}
	return state.reg[*vr.s]
}

func toValueReg(s string) ValueReg {
	i, err := strconv.Atoi(s)
	if err != nil {
		return ValueReg{s: &s}
	}
	return ValueReg{i: &i}
}

type apply func(state *State)

func snd(x ValueReg) apply {
	return func(state *State) {
		state.offset++
		state.frequency = x.get(state)
	}
}

func snd2(x ValueReg) apply {
	return func(state *State) {
		state.offset++
		state.sent++
		v := 0
		if x.s != nil && *x.s == "p" {
			v = state.id
		} else {
			v = x.get(state)
		}
		select {
		case state.output <- v:
		case <-time.After(time.Second):
			state.deadlock = true
		}
	}
}

func set(x ValueReg, y ValueReg) apply {
	return func(state *State) {
		state.offset++
		state.reg[*x.s] = y.get(state)
	}
}

func add(x ValueReg, y ValueReg) apply {
	return func(state *State) {
		state.offset++
		state.reg[*x.s] += y.get(state)
	}
}

func mul(x ValueReg, y ValueReg) apply {
	return func(state *State) {
		state.offset++
		state.reg[*x.s] *= y.get(state)
	}
}

func mod(x ValueReg, y ValueReg) apply {
	return func(state *State) {
		state.offset++
		state.reg[*x.s] = state.reg[*x.s] % y.get(state)
	}
}

func rcv(x ValueReg) apply {
	return func(state *State) {
		state.offset++

		if x.get(state) != 0 {
			state.recovered = true
		}
	}
}

func rcv2(x ValueReg) apply {
	return func(state *State) {
		state.offset++
		select {
		case state.reg[*x.s] = <-state.input:
		case <-time.After(time.Second):
			state.deadlock = true
			return
		}
	}
}

func jgz(x ValueReg, y ValueReg) apply {
	return func(state *State) {
		v := x.get(state)
		if v > 0 {
			state.offset += y.get(state)
		} else {
			state.offset++
		}
	}
}

func fs2(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	var cmds []apply
	for scanner.Scan() {
		s := scanner.Text()
		del := lib.NewDelimiter(s, " ")

		if strings.HasPrefix(s, "snd") {
			cmds = append(cmds, snd2(toValueReg(del.GetString(1))))
		} else if strings.HasPrefix(s, "set") {
			cmds = append(cmds, set(toValueReg(del.GetString(1)), toValueReg(del.GetString(2))))
		} else if strings.HasPrefix(s, "add") {
			cmds = append(cmds, add(toValueReg(del.GetString(1)), toValueReg(del.GetString(2))))
		} else if strings.HasPrefix(s, "mul") {
			cmds = append(cmds, mul(toValueReg(del.GetString(1)), toValueReg(del.GetString(2))))
		} else if strings.HasPrefix(s, "mod") {
			cmds = append(cmds, mod(toValueReg(del.GetString(1)), toValueReg(del.GetString(2))))
		} else if strings.HasPrefix(s, "rcv") {
			cmds = append(cmds, rcv2(toValueReg(del.GetString(1))))
		} else if strings.HasPrefix(s, "jgz") {
			cmds = append(cmds, jgz(toValueReg(del.GetString(1)), toValueReg(del.GetString(2))))
		} else {
			panic(s)
		}
	}

	res := 0
	ch0 := make(chan int, 1_000_000)
	ch1 := make(chan int, 1_000_000)
	go func() {
		offset := 0
		state := State{
			reg:    make(map[string]int),
			id:     0,
			input:  ch0,
			output: ch1,
		}
		for {
			cmds[offset](&state)
			offset = state.offset
			if state.deadlock {
				return
			}
		}
	}()

	wg := sync.WaitGroup{}
	wg.Add(1)
	go func() {
		defer wg.Done()
		offset := 0
		state := State{
			reg:    map[string]int{"p": 1},
			id:     1,
			input:  ch1,
			output: ch0,
		}
		for {
			cmds[offset](&state)
			if state.deadlock {
				res = state.sent
				return
			}
			offset = state.offset
		}
	}()

	wg.Wait()
	return res
}

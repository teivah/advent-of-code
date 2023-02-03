package main

import (
	"bufio"
	"io"

	lib "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	var funcs []apply
	for scanner.Scan() {
		line := scanner.Text()
		del := lib.NewDelimiter(line, " ")
		switch del.GetString(0) {
		case "nop":
			funcs = append(funcs, nop())
		case "acc":
			funcs = append(funcs, acc(del.GetInt(1)))
		case "jmp":
			funcs = append(funcs, jmp(del.GetInt(1)))
		default:
			panic(line)
		}
	}

	s := State{}
	set := make(map[int]bool)
	for {
		set[s.offset] = true
		acc := s.acc
		funcs[s.offset](&s)
		if set[s.offset] {
			return acc
		}
	}

	return -1
}

type State struct {
	offset int
	acc    int
}

type apply func(s *State)

func acc(v int) apply {
	return func(s *State) {
		s.acc += v
		s.offset++
	}
}

func jmp(v int) apply {
	return func(s *State) {
		s.offset += v
	}
}

func nop() apply {
	return func(s *State) {
		s.offset++
	}
}

func fs2(input io.Reader) int {
	lines := lib.ReaderToStrings(input)

	last := -1
	for {
		var funcs []apply
		modified := false
		for i, line := range lines {
			del := lib.NewDelimiter(line, " ")
			switch del.GetString(0) {
			case "nop":
				funcs = append(funcs, nop())
			case "acc":
				funcs = append(funcs, acc(del.GetInt(1)))
			case "jmp":
				if i > last && !modified {
					funcs = append(funcs, nop())
					last = i
					modified = true
				} else {
					funcs = append(funcs, jmp(del.GetInt(1)))
				}
			default:
				panic(line)
			}
		}

		s := State{}
		set := make(map[int]bool)
		terminated := true
		for {
			set[s.offset] = true
			if s.offset >= len(funcs) {
				break
			}
			funcs[s.offset](&s)
			if set[s.offset] {
				terminated = false
				break
			}
		}
		if terminated {
			return s.acc
		}
	}

	return -1
}

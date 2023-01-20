package main

import (
	"bufio"
	"io"

	lib "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader, startState string, steps int) int {
	scanner := bufio.NewScanner(input)
	states := make(map[string]State)
	for scanner.Scan() {
		// Empty line
		scanner.Text()
		id, state := toState(scanner)
		states[id] = state
	}

	checksum := []int{0}
	offset := 0
	id := startState
	for i := 0; i < steps; i++ {
		right := false
		state := states[id]
		if checksum[offset] == 0 {
			checksum[offset] = state.zero.write
			if state.zero.moveRight {
				right = true
			}
			id = state.zero.nextState
		} else {
			checksum[offset] = state.one.write
			if state.one.moveRight {
				right = true
			}
			id = state.one.nextState
		}

		if right {
			offset++
			if offset == len(checksum) {
				checksum = append(checksum, 0)
			}
		} else {
			offset--
			if offset == -1 {
				offset = 0
				checksum = append([]int{0}, checksum...)
			}
		}
	}

	ones := 0
	for _, v := range checksum {
		if v == 1 {
			ones++
		}
	}

	return ones
}

type State struct {
	zero Cmd
	one  Cmd
}

type Cmd struct {
	write     int
	moveRight bool
	nextState string
}

func toCmd(scanner *bufio.Scanner) Cmd {
	// Write
	scanner.Scan()
	s := scanner.Text()
	del := lib.NewDelimiter(s, " ")
	tmp := del.GetString(8)
	write := lib.StringToInt(tmp[:len(tmp)-1])

	// Move
	scanner.Scan()
	s = scanner.Text()
	right := false
	if s[27] == 'r' {
		right = true
	}

	// Continue
	scanner.Scan()
	s = scanner.Text()
	next := string(s[26])

	return Cmd{
		write:     write,
		moveRight: right,
		nextState: next,
	}
}

func toState(scanner *bufio.Scanner) (string, State) {
	scanner.Scan()
	s := scanner.Text()
	del := lib.NewDelimiter(s, " ")
	tmp := del.GetString(2)
	state := tmp[:len(tmp)-1]

	// If the current value is 0
	scanner.Scan()
	s = scanner.Text()
	zero := toCmd(scanner)

	// If the current value is 1
	scanner.Scan()
	s = scanner.Text()
	one := toCmd(scanner)

	return state, State{zero, one}
}

func fs2(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
		line := scanner.Text()
		_ = line
	}

	return 42
}

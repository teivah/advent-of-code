package main

import (
	"fmt"
	"io"
	"strings"

	lib "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader, generations int) int {
	lines := lib.ReaderToStrings(input)

	const shift = 100
	state := strings.Repeat(".", shift) + lines[0][15:] + strings.Repeat(".", shift)

	transformations := make(map[string]string)
	for i := 2; i < len(lines); i++ {
		del := lib.NewDelimiter(lines[i], " ")
		transformations[del.GetString(0)] = del.GetString(2)
	}

	fmt.Printf("%2d: %s\n", 0, state)

	for generation := 0; generation < generations; generation++ {
		state = newState(state, transformations)
		fmt.Printf("%2d: %s\n", generation+1, state)
	}

	sum := 0
	for i := 0; i < len(state); i++ {
		if state[i] == '#' {
			sum += i - shift
		}
	}

	return sum
}

func newState(state string, transformations map[string]string) string {
	res := state[:2]
	for i := 2; i < len(state)-2; i++ {
		s := state[i-2 : i+3]
		if to, exists := transformations[s]; exists {
			//res = res[:len(res)-2] + ".." + to + ".."
			//i += 2
			res += to
		} else {
			//res += state[i : i+1]
			res += "."
		}
	}
	res += state[len(state)-2:]
	return res
}

func transform(s string, foundIdx int, to string) string {
	return s[:foundIdx+2] + to + s[foundIdx+3:]
}

func fs2(input io.Reader, generations int) int {
	lines := lib.ReaderToStrings(input)

	const shift = 1000
	state := strings.Repeat(".", shift) + lines[0][15:] + strings.Repeat(".", shift)

	transformations := make(map[string]string)
	for i := 2; i < len(lines); i++ {
		del := lib.NewDelimiter(lines[i], " ")
		transformations[del.GetString(0)] = del.GetString(2)
	}

	seen := make(map[string][]int)
	for generation := 1; generation <= generations; generation++ {
		state = newState(state, transformations)

		k := toKey(state)
		seen[k] = append(seen[k], generation)
		if generation >= 116 {
			fmt.Println(generation, score(state, shift), generationScore(generation))
		} else {
			fmt.Println(generation, score(state, shift))
		}
	}

	return score(state, shift)
}

func generationScore(generation int) int {
	return (generation-1-116)*73 + 10317
}

func score(state string, shift int) int {
	sum := 0
	for i := 0; i < len(state); i++ {
		if state[i] == '#' {
			sum += i - shift
		}
	}
	return sum
}

func toKey(s string) string {
	l := 0
	for ; l < len(s); l++ {
		if s[l] == '#' {
			break
		}
	}

	r := len(s) - 1
	for ; r >= 0; r-- {
		if s[r] == '#' {
			break
		}
	}

	return s[l : r+1]
}

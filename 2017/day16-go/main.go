package main

import (
	"fmt"
	lib "github.com/teivah/advent-of-code"
	"io"
	"strings"
)

func fs1(initState string, input io.Reader) string {
	s := lib.ReaderToString(input)

	tokens := strings.Split(s, ",")
	var instructions []Instruction
	for _, token := range tokens {
		if token[0] == 's' {
			x := lib.StringToInt(token[1:])
			instructions = append(instructions, Spin{x})
		} else if token[0] == 'x' {
			sep := strings.Index(token, "/")
			a := lib.StringToInt(token[1:sep])
			b := lib.StringToInt(token[sep+1:])
			instructions = append(instructions, Exchange{a, b})
		} else if token[0] == 'p' {
			sep := strings.Index(token, "/")
			a := token[1:sep]
			b := token[sep+1:]
			instructions = append(instructions, Partner{a, b})
		} else {
			panic(token)
		}
	}

	runes := []rune(initState)
	for _, instruction := range instructions {
		runes = instruction.apply(runes)
	}

	return string(runes)
}

type Instruction interface {
	apply([]rune) []rune
}

type Spin struct {
	x int
}

func (s Spin) apply(runes []rune) []rune {
	res := make([]rune, len(runes))
	copy(res, runes)
	a := 0
	b := len(runes) - s.x
	for i := 0; i < len(runes); i++ {
		res[a] = runes[b]
		a++
		b = (b + 1) % len(runes)
	}
	return res
}

type Exchange struct {
	a int
	b int
}

func (e Exchange) apply(runes []rune) []rune {
	runes[e.a], runes[e.b] = runes[e.b], runes[e.a]
	return runes
}

type Partner struct {
	a string
	b string
}

func (p Partner) apply(runes []rune) []rune {
	ia := -1
	ib := -1
	for i := 0; i < len(runes); i++ {
		if ia != -1 && ib != -1 {
			break
		}

		if string(runes[i]) == p.a {
			ia = i
		}
		if string(runes[i]) == p.b {
			ib = i
		}
	}

	runes[ia], runes[ib] = runes[ib], runes[ia]
	return runes
}

func fs2(repeat int, initState string, input io.Reader) string {
	s := lib.ReaderToString(input)

	tokens := strings.Split(s, ",")
	var instructions []Instruction
	for _, token := range tokens {
		if token[0] == 's' {
			x := lib.StringToInt(token[1:])
			instructions = append(instructions, Spin{x})
		} else if token[0] == 'x' {
			sep := strings.Index(token, "/")
			a := lib.StringToInt(token[1:sep])
			b := lib.StringToInt(token[sep+1:])
			instructions = append(instructions, Exchange{a, b})
		} else if token[0] == 'p' {
			sep := strings.Index(token, "/")
			a := token[1:sep]
			b := token[sep+1:]
			instructions = append(instructions, Partner{a, b})
		} else {
			panic(token)
		}
	}

	runes := []rune(initState)
	seen := make(map[string][]int)

	for i := 0; i < repeat; i++ {
		for _, instruction := range instructions {
			runes = instruction.apply(runes)
		}
		s := string(runes)
		seen[s] = append(seen[s], i)

		// I notice they repeat every 60 times
		// 1000000000%60 = 40, so I need the value when i == 39
		if i == 39 {
			return s
		}

		fmt.Println(i, s)
	}

	return s
}

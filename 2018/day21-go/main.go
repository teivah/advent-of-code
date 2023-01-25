package main

import (
	"bufio"
	"io"

	lib "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	scanner.Scan()
	header := scanner.Text()
	ip := lib.StringToInt(header[4:])
	var instructions []apply
	for scanner.Scan() {
		line := scanner.Text()
		del := lib.NewDelimiter(line, " ")
		a := del.GetInt(1)
		b := del.GetInt(2)
		c := del.GetInt(3)
		var instruction apply
		switch del.GetString(0) {
		case "addr":
			instruction = addr(a, b, c)
		case "addi":
			instruction = addi(a, b, c)
		case "mulr":
			instruction = mulr(a, b, c)
		case "muli":
			instruction = muli(a, b, c)
		case "banr":
			instruction = banr(a, b, c)
		case "bani":
			instruction = bani(a, b, c)
		case "borr":
			instruction = borr(a, b, c)
		case "bori":
			instruction = bori(a, b, c)
		case "setr":
			instruction = setr(a, b, c)
		case "seti":
			instruction = seti(a, b, c)
		case "gtir":
			instruction = gtir(a, b, c)
		case "gtri":
			instruction = gtri(a, b, c)
		case "gtrr":
			instruction = gtrr(a, b, c)
		case "eqir":
			instruction = eqir(a, b, c)
		case "eqri":
			instruction = eqri(a, b, c)
		case "eqrr":
			instruction = eqrr(a, b, c)
		default:
			panic(line)
		}
		instructions = append(instructions, instruction)
	}

	reg := make([]int, 6)
	reg[0] = 0
	for {
		if reg[ip] == 28 {
			return reg[4]
		}
		instructions[reg[ip]](reg)
		if reg[ip]+1 >= len(instructions) {
			break
		}
		reg[ip]++
	}

	return 0
}

type apply func(reg []int)

func addr(a, b, c int) apply {
	return func(reg []int) {
		reg[c] = reg[a] + reg[b]
	}
}

func addi(a, b, c int) apply {
	return func(reg []int) {
		reg[c] = reg[a] + b
	}
}

func mulr(a, b, c int) apply {
	return func(reg []int) {
		reg[c] = reg[a] * reg[b]
	}
}

func muli(a, b, c int) apply {
	return func(reg []int) {
		reg[c] = reg[a] * b
	}
}

func banr(a, b, c int) apply {
	return func(reg []int) {
		reg[c] = reg[a] & reg[b]
	}
}

func bani(a, b, c int) apply {
	return func(reg []int) {
		reg[c] = reg[a] & b
	}
}

func borr(a, b, c int) apply {
	return func(reg []int) {
		reg[c] = reg[a] | reg[b]
	}
}

func bori(a, b, c int) apply {
	return func(reg []int) {
		reg[c] = reg[a] | b
	}
}

func setr(a, _, c int) apply {
	return func(reg []int) {
		reg[c] = reg[a]
	}
}

func seti(a, _, c int) apply {
	return func(reg []int) {
		reg[c] = a
	}
}

func gtir(a, b, c int) apply {
	return func(reg []int) {
		if a > reg[b] {
			reg[c] = 1
		} else {
			reg[c] = 0
		}
	}
}

func gtri(a, b, c int) apply {
	return func(reg []int) {
		if reg[a] > b {
			reg[c] = 1
		} else {
			reg[c] = 0
		}
	}
}

func gtrr(a, b, c int) apply {
	return func(reg []int) {
		if reg[a] > reg[b] {
			reg[c] = 1
		} else {
			reg[c] = 0
		}
	}
}

func eqir(a, b, c int) apply {
	return func(reg []int) {
		if a == reg[b] {
			reg[c] = 1
		} else {
			reg[c] = 0
		}
	}
}

func eqri(a, b, c int) apply {
	return func(reg []int) {
		if reg[a] == b {
			reg[c] = 1
		} else {
			reg[c] = 0
		}
	}
}

func eqrr(a, b, c int) apply {
	return func(reg []int) {
		if reg[a] == reg[b] {
			reg[c] = 1
		} else {
			reg[c] = 0
		}
	}
}

func fs2(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	scanner.Scan()
	header := scanner.Text()
	ip := lib.StringToInt(header[4:])
	var instructions []apply
	for scanner.Scan() {
		line := scanner.Text()
		del := lib.NewDelimiter(line, " ")
		a := del.GetInt(1)
		b := del.GetInt(2)
		c := del.GetInt(3)
		var instruction apply
		switch del.GetString(0) {
		case "addr":
			instruction = addr(a, b, c)
		case "addi":
			instruction = addi(a, b, c)
		case "mulr":
			instruction = mulr(a, b, c)
		case "muli":
			instruction = muli(a, b, c)
		case "banr":
			instruction = banr(a, b, c)
		case "bani":
			instruction = bani(a, b, c)
		case "borr":
			instruction = borr(a, b, c)
		case "bori":
			instruction = bori(a, b, c)
		case "setr":
			instruction = setr(a, b, c)
		case "seti":
			instruction = seti(a, b, c)
		case "gtir":
			instruction = gtir(a, b, c)
		case "gtri":
			instruction = gtri(a, b, c)
		case "gtrr":
			instruction = gtrr(a, b, c)
		case "eqir":
			instruction = eqir(a, b, c)
		case "eqri":
			instruction = eqri(a, b, c)
		case "eqrr":
			instruction = eqrr(a, b, c)
		default:
			panic(line)
		}
		instructions = append(instructions, instruction)
	}

	reg := make([]int, 6)
	reg[0] = 0
	set := make(map[int]struct{})
	latest := 0
	for {
		if reg[ip] == 28 {
			if _, exists := set[reg[4]]; exists {
				return latest
			}
			set[reg[4]] = struct{}{}
			latest = reg[4]
		}
		instructions[reg[ip]](reg)
		if reg[ip]+1 >= len(instructions) {
			break
		}
		reg[ip]++
	}

	return 0
}

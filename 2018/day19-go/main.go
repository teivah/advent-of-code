package main

import (
	"bufio"
	"fmt"
	"io"

	lib "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader, reg []int) int {
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
		}
		instructions = append(instructions, instruction)
	}

	for {
		fmt.Println(reg[ip])
		instructions[reg[ip]](reg)
		if reg[ip]+1 >= len(instructions) {
			break
		}
		reg[ip]++
	}

	return reg[0]
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

func fs2() int {
	r0 := 1
	r1 := 0
	r2 := 0
	r3 := 0
	ip := 0
	r5 := 0

	// addi 4 16 4
	ip = ip + 16
	// seti 1 5 1
	r1 = 1
	// seti 1 2 2
	r2 = 1
	// mulr 1 2 3
	r3 = r1 * r2
	// eqrr 3 5 3
	if r3 == r5 {
		r3 = 1
	} else {
		r3 = 0
	}
	// addr 3 4 4
	ip = r3 + ip
	// addi 4 1 4
	ip++
	// addr 1 0 0
	r0 += r1
	// addi 2 1 2
	r2 += 1
	// gtrr 2 5 3
	if r2 > r5 {
		r3 = 1
	} else {
		r3 = 0
	}
	// addr 4 3 4
	ip += r3
	// seti 2 7 4
	ip = r2
	// addi 1 1 1
	r1++
	// gtrr 1 5 3
	if r1 > r5 {
		r3 = 1
	} else {
		r3 = 0
	}
	// addr 3 4 4
	ip += r3
	// seti 1 9 4
	ip = 1
	// mulr 4 4 4
	ip *= ip
	// addi 5 2 5
	r5 += 2
	// mulr 5 5 5
	r5 *= r5
	// mulr 4 5 5
	r5 *= ip
	// muli 5 11 5
	r5 *= 11
	// addi 3 1 3
	r3 += 1
	// mulr 3 4 3
	r3 *= ip
	// addi 3 18 3
	r3 += 18
	// addr 5 3 5
	r5 += r3
	// addr 4 0 4
	ip += r0
	// seti 0 3 4
	ip = 0
	// setr 4 2 3
	r3 = ip
	// mulr 3 4 3
	r3 *= ip
	// addr 4 3 3
	r3 += ip
	// mulr 4 3 3
	r3 *= ip
	// muli 3 14 3
	r3 *= 14
	// mulr 3 4 3
	r3 *= ip
	// addr 5 3 5
	r5 *= r3
	// seti 0 4 0
	r0 = 0
	// seti 0 5 4
	ip = 0

	return 0
}

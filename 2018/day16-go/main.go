package main

import (
	"bufio"
	"fmt"
	"io"
	"strconv"
	"strings"

	lib "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	samples := parseSamples(lib.ReaderToStrings(input))

	sum := 0
	fmt.Println(len(samples))
	cmds := getCommands()
	for _, sample := range samples {
		options := 0
		for _, cmd := range cmds {
			regs := copyRegs(sample.before)
			cmd(sample.a, sample.b, sample.c)(regs)

			if areRegsEqual(regs, sample.after) {
				options++
				//fmt.Println(k)
			}
		}
		if options >= 3 {
			sum++
		}
	}

	return sum
}

func areRegsEqual(a []int, b []int) bool {
	for i := 0; i < len(a); i++ {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

func copyRegs(s []int) []int {
	res := make([]int, len(s))
	copy(res, s)
	return res
}

func getCommands() map[string]func(int, int, int) apply {
	return map[string]func(int, int, int) apply{
		"addr": addr,
		"addi": addi,
		"mulr": mulr,
		"muli": muli,
		"banr": banr,
		"bani": bani,
		"borr": borr,
		"bori": bori,
		"setr": setr,
		"seti": seti,
		"gtir": gtir,
		"gtri": gtri,
		"gtrr": gtrr,
		"eqir": eqir,
		"eqri": eqri,
		"eqrr": eqrr,
	}
}

func parseSamples(lines []string) []Sample {
	var samples []Sample
	for i := 0; i < len(lines); i++ {
		if len(lines[i]) == 0 {
			break
		}
		v := lines[i][9 : len(lines[i])-1]
		before := listStringsToListInts(v, ", ")

		cmd := listStringsToListInts(lines[i+1], " ")

		v = lines[i+2][9 : len(lines[i+2])-1]
		after := listStringsToListInts(v, ", ")

		samples = append(samples, Sample{
			before: before,
			after:  after,
			opcode: cmd[0],
			a:      cmd[1],
			b:      cmd[2],
			c:      cmd[3],
		})
		i += 3
	}
	return samples
}

func listStringsToListInts(s, del string) []int {
	list := strings.Split(s, del)

	res := make([]int, len(list))
	for i, v := range list {
		x, err := strconv.Atoi(v)
		if err != nil {
			panic(err)
		}
		res[i] = x
	}
	return res
}

type Sample struct {
	before []int
	after  []int
	opcode int
	a      int
	b      int
	c      int
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
	for scanner.Scan() {
		line := scanner.Text()
		_ = line
	}

	return 42
}

package main

import (
	"io"
	"strconv"
	"strings"

	lib "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	samples := parseSamples(lib.ReaderToStrings(input))

	sum := 0
	cmds := getCommands()
	for _, sample := range samples {
		options := 0
		for _, cmd := range cmds {
			regs := copyRegs(sample.before)
			cmd(sample.a, sample.b, sample.c)(regs)

			if areRegsEqual(regs, sample.after) {
				options++
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
	lines := lib.ReaderToStrings(input)
	samples := parseSamples(lines)

	type Cmd struct {
		name string
		f    func(int, int, int) apply
	}

	options := make(map[int][]Cmd)
	for i := 0; i < 16; i++ {
		for k, v := range getCommands() {
			options[i] = append(options[i], Cmd{
				name: k,
				f:    v,
			})
		}
	}

	for _, sample := range samples {
		cmds := options[sample.opcode]
		var res []Cmd
		for _, cmd := range cmds {
			regs := copyRegs(sample.before)
			cmd.f(sample.a, sample.b, sample.c)(regs)

			if areRegsEqual(regs, sample.after) {
				res = append(res, cmd)
			}
		}
		options[sample.opcode] = res
	}

	res := make(map[int]Cmd)
	var q []Cmd
	for k, v := range options {
		if len(v) == 1 {
			res[k] = v[0]
			q = append(q, v[0])
			delete(options, k)
		}
	}

	for len(q) != 0 {
		root := q[0]
		q = q[1:]

		// Filter out cmd occurences
		newOptions := make(map[int][]Cmd)
		for k, cmds := range options {
			var res []Cmd
			for _, cmd := range cmds {
				if cmd.name != root.name {
					res = append(res, cmd)
				}
			}
			newOptions[k] = res
		}

		options = newOptions
		for k, v := range options {
			if len(v) == 1 {
				res[k] = v[0]
				q = append(q, v[0])
				delete(options, k)
			}
		}
	}

	i := 0
	for ; i < len(lines); i++ {
		if lines[i] == "" && lines[i+1] == "" {
			i += 3
			break
		}
	}

	reg := make([]int, 4)
	for ; i < len(lines); i++ {
		cmd := listStringsToListInts(lines[i], " ")
		res[cmd[0]].f(cmd[1], cmd[2], cmd[3])(reg)
	}

	return reg[0]
}

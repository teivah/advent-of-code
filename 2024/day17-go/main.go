package main

import (
	"fmt"
	"io"
	"math"
	"strconv"
	"strings"
	"sync"

	"github.com/teivah/go-aoc"
)

/*

Ins:
- 0: adv
numerator: A
divisor: 2^combo
Result truncated and then written to A
- 1: blx
bitwise XOR of B and literal => B
- 2: bst
combo % 8 => B
- 3: jnz
nothing if A is 0
otherwise, jumps to the IP of the literal operand (no IP increased after)
- 4: bxc
XOR of B and C => B
- 5: out
combo % 7 => output if multiple, separated by commas
- 6: bdv
same as adv but store in B
- 7: cdv
same as adv but store in C
*/

type opcode uint8

const (
	adv opcode = iota
	blx
	bst
	jnz
	bxc
	out
	bdv
	cdv
)

func fs1(input io.Reader) string {
	comp := newComputer(aoc.ReaderToStrings(input))
	for !comp.isStopped() {
		comp.cycle()
	}
	return strings.Join(comp.stdout, ",")
}

type computer struct {
	a         int
	b         int
	c         int
	program   []int
	ip        int
	stdout    []string
	stdoutIdx int
	i         int
}

func newComputer(lines []string) *computer {
	return &computer{
		a:       aoc.StringToInt(aoc.Substring(lines[0], ": ")),
		b:       aoc.StringToInt(aoc.Substring(lines[1], ": ")),
		c:       aoc.StringToInt(aoc.Substring(lines[2], ": ")),
		program: aoc.NewDelimiter(aoc.Substring(lines[4], ": "), ",").GetInts(),
	}
}

func (c *computer) copy() *computer {
	return &computer{
		a:       c.a,
		b:       c.b,
		c:       c.c,
		program: aoc.SliceCopy(c.program),
	}
}

func (c *computer) cycle() {
	op := opcode(c.program[c.ip])
	switch op {
	default:
		panic(op)
	case adv:
		c.adv()
	case blx:
		c.blx()
	case bst:
		c.bst()
	case jnz:
		c.jnz()
	case bxc:
		c.bxc()
	case out:
		c.out()
	case bdv:
		c.bdv()
	case cdv:
		c.cdv()
	}
}

func (c *computer) adv() {
	c.a = c.a / int(math.Pow(float64(2), float64(c.combo())))
	c.ip += 2
}

func (c *computer) blx() {
	c.b = c.b ^ c.literal()
	c.ip += 2
}

func (c *computer) bst() {
	c.b = aoc.Mod(c.combo(), 8)
	c.ip += 2
}

func (c *computer) jnz() {
	if c.a == 0 {
		c.ip += 2
	} else {
		c.ip = c.literal()
	}
}

func (c *computer) bxc() {
	c.b = c.b ^ c.c
	c.ip += 2
}

func (c *computer) out() {
	n := c.combo()
	v := aoc.Mod(n, 8)
	r := strconv.Itoa(v)[0]
	c.stdout = append(c.stdout, string(r))
	c.ip += 2
}

func substr(s string, i int) string {
	v := s[len(s)-i:]
	return v
}

var mu sync.Mutex

var visited = map[string]bool{}

func (c *computer) bdv() {
	n := c.combo()
	c.b = c.a / int(math.Pow(float64(2), float64(n)))
	c.ip += 2
}

func (c *computer) cdv() {
	n := c.combo()
	c.c = c.a / int(math.Pow(float64(2), float64(n)))
	c.ip += 2
}

func (c *computer) literal() int {
	return c.program[c.ip+1]
}

func (c *computer) combo() int {
	operand := c.program[c.ip+1]
	if operand >= 0 && operand <= 3 {
		return operand
	}
	switch operand {
	default:
		panic(operand)
	case 4:
		return c.a
	case 5:
		return c.b
	case 6:
		return c.c
	}
}

func (c *computer) isStopped() bool {
	return c.ip >= len(c.program)
}

func fs2(input io.Reader) int {
	comp := newComputer(aoc.ReaderToStrings(input))
	ins := len(comp.program)

	n := 9
	search := comp.program[len(comp.program)-n:]
	var checks []string
	for i := 0; i < digits(3*n); i++ {
		var res []int
		var c int
		a := i
		found := true
		for j := 0; j < n; j++ {
			c, a = calc(a)
			res = append(res, c)
			if search[j] != c {
				found = false
				break
			}
		}
		if found {
			checks = append(checks, binary(i, n))
		}
	}

	m := digits((ins - n) * 3)
	search = comp.program
	res := math.MaxInt
	for _, check := range checks {
		for i := 0; i <= m; i++ {
			d := convert(check, i)
			a := d
			var c int
			found := true
			for j := 0; j < 16; j++ {
				c, a = calc(a)
				if search[j] != c {
					found = false
					break
				}
			}
			if found && a == 0 {
				res = min(res, d)
			}
		}
	}
	return res
}

func (c *computer) isOutputProgram() bool {
	return c.stdoutIdx == len(c.program)
}

func calc(a int) (int, int) {
	b := a % 8
	b = b ^ 1
	c := a / int(math.Pow(float64(2), float64(b)))
	a = a / 8
	b = b ^ 4
	b = b ^ c
	return b % 8, a
}

func convert(prefix string, i int) int {
	s := fmt.Sprintf("%s%021b", prefix, i)
	d, err := strconv.ParseInt(s, 2, 0)
	if err != nil {
		panic(err)
	}
	return int(d)
}

func binary(i, n int) string {
	s := fmt.Sprintf("%048b", i)
	return s[len(s)-(n*3):]
}

func digits(x int) int {
	s := strings.Repeat("1", x)
	result, _ := strconv.ParseInt(s, 2, 64)
	return int(result)
}

package main

import (
	"fmt"
	"io"
	"math"
	"strconv"
	"strings"
	"sync"

	"github.com/teivah/go-aoc"
	"golang.org/x/sync/errgroup"
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

	part2 bool
	stop  bool
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
		part2:   c.part2,
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
	n := c.combo()
	c.a = c.a / int(math.Pow(float64(2), float64(n)))
	c.ip += 2
}

func (c *computer) blx() {
	n := c.literal()
	c.b = c.b ^ n
	c.ip += 2
}

func (c *computer) bst() {
	n := c.combo()
	c.b = aoc.Mod(n, 8)
	c.ip += 2
}

func (c *computer) jnz() {
	if c.a == 0 {
		c.ip += 2
	} else {
		n := c.literal()
		c.ip = n
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
	if !c.part2 {
		c.stdout = append(c.stdout, string(r))
	} else {
		first := c.program[c.stdoutIdx]
		if first != v {
			c.stdoutIdx = -1
			c.stop = true
		} else {
			c.stdoutIdx++
		}

		x := 1
		if !c.stop && c.stdoutIdx == x {
			s := fmt.Sprintf("%064b", c.i)
			sub := substr(s, x*3)
			mu.Lock()
			if !visited[sub] {
				visited[sub] = true
				//fmt.Printf("%2d %32d %064b\n", c.stdoutIdx, c.i, c.i)
				fmt.Printf("%d %s\n", first, sub)
			}
			mu.Unlock()
		}
	}
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
	return c.ip >= len(c.program) || c.stop
}

func fs2(input io.Reader) int {
	eg := errgroup.Group{}
	eg.SetLimit(1)

	lines := aoc.ReaderToStrings(input)
	zero := newComputer(lines)
	zero.part2 = true
	for i := 0; ; i += 1 {
		if i < 0 {
			return 0
		}
		//if i%10_000_000 == 0 {
		//	fmt.Println(i)
		//}
		eg.Go(func() error {
			comp := zero.copy()
			comp.a = i
			comp.i = i
			comp.part2 = true
			for !comp.isStopped() {
				comp.cycle()
			}
			if comp.isOutputProgram() {
				panic(i)
			}
			return nil
		})
	}
}

func fs21(input io.Reader) int {
	lines := aoc.ReaderToStrings(input)
	zero := newComputer(lines)
	zero.part2 = true
	for i := 1310096632362; ; i += 10794 {
		if i%10_000_000 == 0 {
			fmt.Println(i)
		}
		comp := zero.copy()
		comp.a = i
		comp.i = i
		comp.part2 = true
		for !comp.isStopped() {
			comp.cycle()
		}
		if comp.isOutputProgram() {
			panic(i)
		}
	}
}

func (c *computer) solve() {

}

func (c *computer) isOutputProgram() bool {
	return c.stdoutIdx == len(c.program)
}

func solve() {
	eg := errgroup.Group{}
	eg.SetLimit(16)

	found := []int{2, 4, 1, 1, 7, 5, 0, 3, 1, 4, 4, 5, 5, 5, 3, 0}
	//found := []int{0, 3, 5, 4, 3, 0}
	//for i := 0; ; i++ {
	for i := 140737488355328; i <= 281474976710655; i += 64 {
		if i%10_000_000 == 0 {
			fmt.Println(i)
		}

		//eg.Go(func() error {
		a := i
		idx := 0
		for {
			var prt int
			prt, a = calc(a)
			if found[idx] != prt {
				break
			}
			idx++
			if idx == 10 {
				fmt.Printf("%2d %32d %48b\n", idx, i, i)
			}
			if idx == len(found) && a == 0 {
				panic(i)
			}
			if idx == len(found) {
				//return nil
				break
			}
		}
		//	return nil
		//})
	}
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

func pow(a, b int) int {
	return int(math.Pow(float64(a), float64(b)))
}

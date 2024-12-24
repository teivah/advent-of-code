package main

import (
	"fmt"
	"io"
	"slices"
	"sort"
	"strconv"
	"strings"

	"github.com/teivah/go-aoc"
)

func fs1(input io.Reader) int {
	gates, operations := parse(input)
	i := 0
	for len(operations) != 0 {
		i++
		operations = slices.DeleteFunc(operations, func(o operation) bool {
			va, ok := gates[o.a]
			if !ok {
				return false
			}
			vb, ok := gates[o.b]
			if !ok {
				return false
			}
			var result int
			switch o.op {
			default:
				panic(o.op)
			case or:
				result = va | vb
			case and:
				result = va & vb
			case xor:
				result = va ^ vb
			}
			gates[o.c] = result
			return true
		})
	}
	fmt.Println(i)

	m := make(map[int]int)
	for gate, v := range gates {
		if gate[0] == 'z' {
			id := aoc.StringToInt(gate[1:])
			m[id] = v
		}
	}
	res := make([]rune, len(m))
	for id, v := range m {
		s := '0'
		if v == 1 {
			s = '1'
		}
		res[len(res)-id-1] = s
	}

	number, err := strconv.ParseInt(string(res), 2, 64)
	if err != nil {
		panic(err)
	}
	return int(number)
}

type operator uint8

const (
	or operator = iota
	and
	xor
)

type operation struct {
	a  string
	b  string
	c  string
	op operator
}

func parse(input io.Reader) (map[string]int, []operation) {
	groups := aoc.StringGroups(aoc.ReaderToStrings(input))

	m := make(map[string]int)
	for _, line := range groups[0] {
		del := aoc.NewDelimiter(line, ": ")
		m[del.GetString(0)] = del.GetInt(1)
	}

	var operations []operation
	for _, line := range groups[1] {
		del := aoc.NewDelimiter(line, " ")
		var op operator
		switch del.GetString(1) {
		default:
			panic(del.GetString(1))
		case "OR":
			op = or
		case "AND":
			op = and
		case "XOR":
			op = xor
		}
		operations = append(operations, operation{
			a:  del.GetString(0),
			b:  del.GetString(2),
			c:  del.GetString(4),
			op: op,
		})
	}

	return m, operations
}

func fs2(input io.Reader) int {
	gates, operations := parse(input)
	_, _ = gates, operations
	digits := getMax('x', gates)
	maxValue := fromBinary(strings.Repeat("1", digits))
	x := maxValue
	y := maxValue
	if x > maxValue || y > maxValue {
		panic(fmt.Sprintf("x=%d, y=%d", x, y))
	}

	var ids []string
	for id := range gates {
		ids = append(ids, id)
	}
	sort.Strings(ids)

	for i := 0; i < len(operations); i++ {
		fmt.Println("i: ", i)
		for j := i + 1; j < len(operations); j++ {
			for k := j + 1; k < len(operations); k++ {
				for l := k + 1; l < len(operations); l++ {
					for m := l + 1; m < len(operations); m++ {
						for n := m + 1; n < len(operations); n++ {
							fmt.Println("x: ", n)
							for o := n + 1; o < len(operations); o++ {
								for p := o + 1; p < len(operations); p++ {
									g := aoc.MapCopy(gates)
									op := aoc.SliceCopy(operations)
									op[i].c, op[j].c = op[j].c, op[i].c
									op[k].c, op[l].c = op[l].c, op[k].c
									op[m].c, op[n].c = op[n].c, op[m].c
									op[o].c, op[p].c = op[p].c, op[o].c
									if check(digits, x, y, g, op) {
										fmt.Println(operations[i], operations[j], operations[k], operations[l])
									}
								}
							}
						}
					}
				}
			}
		}
	}

	fmt.Printf("%v\n", check(digits, x, y, gates, operations))
	return 0
}

func check(digits, x, y int, gates map[string]int, operations []operation) bool {
	expected := x + y
	z, finished := getZ(digits, x, y, gates, operations)
	if !finished {
		return false
	}
	return z == expected
}

func getZ(digits, x, y int, gates map[string]int, operations []operation) (int, bool) {
	xb, yb := toBinary(digits, x), toBinary(digits, y)
	for id := range gates {
		n := aoc.StringToInt(id[1:])
		if id[0] == 'x' {
			gates[id] = 1
			gates[id] = aoc.StringToInt(string(xb[len(xb)-n-1]))
		} else if id[0] == 'y' {
			gates[id] = 1
			gates[id] = aoc.StringToInt(string(yb[len(yb)-n-1]))
		}
	}
	gates, finished := perform(gates, operations)
	if !finished {
		return 0, false
	}
	return getValue(gates), true
}

func getValue(gates map[string]int) int {
	m := make(map[int]int)
	for gate, v := range gates {
		if gate[0] == 'z' {
			id := aoc.StringToInt(gate[1:])
			m[id] = v
		}
	}
	res := make([]rune, len(m))
	for id, v := range m {
		s := '0'
		if v == 1 {
			s = '1'
		}
		res[len(res)-id-1] = s
	}
	return fromBinary(string(res))
}

func getMax(r rune, gates map[string]int) int {
	m := 0
	for id := range gates {
		if rune(id[0]) == r {
			m = max(m, aoc.StringToInt(id[1:]))
		}
	}
	return m + 1
}

func perform(gates map[string]int, operations []operation) (map[string]int, bool) {
	prev := len(operations)
	for len(operations) != 0 {
		operations = slices.DeleteFunc(operations, func(o operation) bool {
			va, ok := gates[o.a]
			if !ok {
				return false
			}
			vb, ok := gates[o.b]
			if !ok {
				return false
			}
			var result int
			switch o.op {
			default:
				panic(o.op)
			case or:
				result = va | vb
			case and:
				result = va & vb
			case xor:
				result = va ^ vb
			}
			gates[o.c] = result
			return true
		})
		if len(operations) == prev {
			return nil, false
		}
		prev = len(operations)
	}
	return gates, true
}

func fromBinary(s string) int {
	value, err := strconv.ParseInt(s, 2, 64)
	if err != nil {
		panic(err)
	}
	return int(value)
}

func toBinary(digits, i int) string {
	return fmt.Sprintf("%0*b", digits, i)
}

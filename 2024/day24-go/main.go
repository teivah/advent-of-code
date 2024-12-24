package main

import (
	"fmt"
	"io"
	"slices"
	"sort"
	"strconv"

	"github.com/teivah/go-aoc"
)

func fs1(input io.Reader) int {
	gates, operations := parse(input)

	set := make(map[string]struct{})
	edges := make(map[string][]string)
	for id := range gates {
		set[id] = struct{}{}
	}
	for _, op := range operations {
		set[op.c] = struct{}{}
		edges[op.a] = append(edges[op.a], op.c)
		edges[op.b] = append(edges[op.b], op.c)
	}
	vertices := aoc.MapKeysToSlice(set)

	t := aoc.TopologicalSort(vertices, func(s string) []string {
		return edges[s]
	})
	s := make(map[string]int)
	for i, v := range t {
		s[v] = i
	}

	sort.Slice(operations, func(i, j int) bool {
		if s[operations[i].a] < s[operations[j].a] &&
			s[operations[i].b] < s[operations[j].b] {
			return true
		}
		return false
	})

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

func newOperation(a, b, c string, op operator) operation {
	if b < a {
		a, b = b, a
	}
	return operation{
		a:  a,
		b:  b,
		c:  c,
		op: op,
	}
}

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
		o := newOperation(del.GetString(0), del.GetString(2), del.GetString(4), op)
		operations = append(operations, o)
	}

	return m, operations
}

func fs2(input io.Reader) int {
	_, operations := parse(input)

	src := make(map[string][]operation)
	dst := make(map[string][]operation)
	for _, o := range operations {
		src[o.a] = append(src[o.a], o)
		src[o.b] = append(src[o.b], o)
		dst[o.c] = append(dst[o.c], o)
	}

	c := "kqn"
	// This part will panic every time there's an input issue
	for i := 1; i <= 44; i++ {
		x := format("x", i)
		y := format("y", i)
		z := format("z", i)

		op, found := find(operations, func(o operation) bool {
			return o.a == x && o.b == y && o.op == xor
		})
		if !found {
			stop(i, c, "a")
		}
		a := op.c

		_, found = find(operations, func(o operation) bool {
			if o.op != xor {
				return false
			}
			if o.c != z {
				return false
			}
			if (o.a == a && o.b == c) || (o.a == c || o.b == a) {
				return true
			}
			return false
		})
		if !found {
			stop(i, c, "z")
		}

		op, found = find(operations, func(o operation) bool {
			if o.op != and {
				return false
			}
			if o.a == x && o.b == y {
				return true
			}
			return false
		})
		if !found {
			stop(i, c, "b")
		}
		b := op.c

		op, found = find(operations, func(o operation) bool {
			if o.op != and {
				return false
			}
			if (o.a == a && o.b == c) || (o.a == c || o.b == a) {
				return true
			}
			return false
		})
		if !found {
			stop(i, c, "d")
		}
		d := op.c

		op, found = find(operations, func(o operation) bool {
			if o.op != or {
				return false
			}
			if (o.a == b && o.b == d) || (o.a == d || o.b == b) {
				return true
			}
			return false
		})
		if !found {
			stop(i, c, "c")
		}
		c = op.c
		fmt.Println(c)
	}
	return 0
}

func stop(i int, carry string, msg any) {
	panic(fmt.Sprintf("%d (carry=%s): %v", i, carry, msg))
}

func find(operations []operation, f func(o operation) bool) (operation, bool) {
	for _, o := range operations {
		if f(o) {
			return o, true
		}
	}
	return operation{}, false
}

func format(v string, i int) string {
	return fmt.Sprintf("%s%02d", v, i)
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

func add(x, y int) int {
	n := 64
	xb := toBinary(n, x)
	yb := toBinary(n, y)
	carry := 0
	res := make([]rune, n+1)
	for i := n - 1; i >= 0; i-- {
		xv := aoc.StringToInt(string(xb[i]))
		yv := aoc.StringToInt(string(yb[i]))
		v := xv ^ yv ^ carry
		if v == 0 {
			res[i+1] = '0'
		} else {
			res[i+1] = '1'
		}
		carry = (xv & yv) | (carry & (xv ^ yv))
	}
	if carry == 0 {
		res[0] = '0'
	} else {
		res[0] = '1'
	}

	return fromBinary(string(res))
}

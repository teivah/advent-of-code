package main

import (
	"bufio"
	"fmt"
	"io"
	"strings"

	lib "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	transformations := make(map[string]Transformation)
	for scanner.Scan() {
		t := toTransformation(scanner.Text())
		transformations[t.to.name] = t
	}

	v := min("FUEL", 1, transformations)
	return v
}

func min(name string, count int, transformations map[string]Transformation) int {
	inDegree := make(map[string]int)
	for k, t := range transformations {
		inDegree[k] = 0
		for _, v := range t.from {
			inDegree[v.name] = 0
		}
	}

	for _, t := range transformations {
		for _, v := range t.from {
			inDegree[v.name]++
		}
	}

	var q []string
	for k, v := range inDegree {
		if v == 0 {
			q = append(q, k)
		}
	}

	level := 0
	levels := make(map[string]int)
	for len(q) != 0 {
		v := len(q)
		for i := 0; i < v; i++ {
			s := q[0]
			q = q[1:]
			levels[s] = level

			for _, unit := range transformations[s].from {
				inDegree[unit.name]--
				if inDegree[unit.name] == 0 {
					q = append(q, unit.name)
				}
			}
		}
		level++
	}

	m := needed(name, count, transformations)

	level = 1
	for {
		m2 := make(map[string]int)
		for name, count := range m {
			if levels[name] != level {
				m2[name] += count
				continue
			}

			m3 := needed(name, count, transformations)
			for k, v := range m3 {
				m2[k] += v
			}
		}

		m = m2
		if len(m) == 1 {
			if v, exists := m["ORE"]; exists {
				return v
			}
		}
		level++
	}
}

func needed(name string, count int, transformations map[string]Transformation) map[string]int {
	t := transformations[name]
	n := 1
	for ; ; n++ {
		if n*t.to.count >= count {
			break
		}
	}

	sub := make(map[string]int)
	for _, unit := range t.from {
		sub[unit.name] += unit.count * n
	}
	return sub
}

func cpy(remaining map[string]Unit) map[string]Unit {
	res := make(map[string]Unit, len(remaining))
	for k, v := range remaining {
		res[k] = v
	}
	return res
}

func add(remaining map[string]Unit, count int, name string) {
	unit, exists := remaining[name]
	if !exists {
		remaining[name] = Unit{
			name:  name,
			count: count,
		}
		return
	}

	unit.count += count
	remaining[name] = unit
}

func del(remaining map[string]Unit, count int, name string) {
	unit := remaining[name]
	if unit.count == count {
		delete(remaining, name)
		return
	}
	unit.count -= count
	remaining[name] = unit
}

func toTransformation(s string) Transformation {
	s = strings.TrimSpace(s)

	start := 0
	var from []Unit
	for {
		if s[start] == '=' {
			break
		}

		i := start + 1
		for ; ; i++ {
			r := rune(s[i])
			if !isDigit(r) {
				break
			}
		}
		count := lib.StringToInt(s[start:i])

		start = i + 1
		i = start + 1
		for ; ; i++ {
			r := rune(s[i])
			if !isLetter(r) {
				break
			}
		}
		name := s[start:i]
		from = append(from, Unit{
			name:  name,
			count: count,
		})
		start = i
		if s[start] == ',' {
			start = i + 2
		} else {
			start = i + 4
			break
		}
	}

	del := lib.NewDelimiter(s[start:], " ")
	return Transformation{
		from: from,
		to: Unit{
			count: del.GetInt(0),
			name:  del.GetString(1),
		},
	}
}

func isDigit(r rune) bool {
	return r >= '0' && r <= '9'
}

func isLetter(r rune) bool {
	return r >= 'A' && r <= 'Z'
}

type Transformation struct {
	from []Unit
	to   Unit
}

type Unit struct {
	name  string
	count int
}

func (u Unit) String() string {
	return fmt.Sprintf("%s:%v", u.name, u.count)
}

func fs2(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	transformations := make(map[string]Transformation)
	for scanner.Scan() {
		t := toTransformation(scanner.Text())
		transformations[t.to.name] = t
	}

	one := min("FUEL", 1, transformations)
	const v = 1_000_000_000_000
	return int(
		(float64(v) / float64(one)) *
			float64(v) / float64(min("FUEL", v/one, transformations)))
}

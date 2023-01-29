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

	v := min(Unit{"FUEL", 1}, transformations, make(map[string]Unit))
	return v
}

func min(unit Unit, transformations map[string]Transformation, remaining map[string]Unit) int {
	if unit.name == "ORE" {
		return unit.count
	}

	if v, exists := remaining[unit.name]; exists {
		if v.count >= unit.count {
			del(remaining, v.count-unit.count, unit.name)
			return 0
		}
	}

	t := transformations[unit.name]

	n := 1
	for ; ; n++ {
		if n*t.to.count >= unit.count {
			break
		}
	}
	// We need to repeat the process n times

	v := n*t.to.count - unit.count

	sum := 0
	for _, needed := range t.from {
		count := min(needed, transformations, remaining)
		sum += count
	}

	add(remaining, v, unit.name)
	return sum
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
	for scanner.Scan() {
		line := scanner.Text()
		_ = line
	}

	return 42
}

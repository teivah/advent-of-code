package main

import (
	"bufio"
	"io"

	aoc "github.com/teivah/advent-of-code"
)

type SpringType int

const (
	springTypeUnknown SpringType = iota
	springTypeOperational
	springTypeDamaged
)

type Entry struct {
	springTypesLength int
	numbersLength     int
	remaining         int
}

func fs1(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	res := 0
	for scanner.Scan() {
		springTypes, numbers := parse(scanner.Text())
		res += counter{cache: make(map[Entry]int)}.
			countArrangements(springTypes, numbers, -1)
	}
	return res
}

func parse(line string) ([]SpringType, []int) {
	del := aoc.NewDelimiter(line, " ")
	var springs []SpringType
	s := del.GetString(0)
	for i := 0; i < len(s); i++ {
		c := s[i]
		switch c {
		case '?':
			springs = append(springs, springTypeUnknown)
		case '.':
			springs = append(springs, springTypeOperational)
		case '#':
			springs = append(springs, springTypeDamaged)
		default:
			panic(string(c))
		}
	}

	return springs, aoc.NewDelimiter(del.GetString(1), ",").GetInts()
}

type counter struct {
	cache map[Entry]int
}

func (c counter) countArrangements(springTypes []SpringType, numbers []int, remaining int) int {
	if len(springTypes) == 0 {
		if len(numbers) == 0 || (len(numbers) == 1 && remaining == 0) {
			return 1
		}
		return 0
	}

	entry := Entry{springTypesLength: len(springTypes), numbersLength: len(numbers), remaining: remaining}
	if v, exists := c.cache[entry]; exists {
		return v
	}

	springType := springTypes[0]
	res := 0
	switch springType {
	case springTypeUnknown:
		switch remaining {
		case -1:
			with := 0
			if len(numbers) != 0 {
				with = c.countArrangements(springTypes[1:], numbers, numbers[0]-1)
			}
			without := c.countArrangements(springTypes[1:], numbers, -1)
			res = with + without
		case 0:
			res = c.countArrangements(springTypes[1:], numbers[1:], -1)
		default:
			res = c.countArrangements(springTypes[1:], numbers, remaining-1)
		}
	case springTypeOperational:
		switch remaining {
		case -1:
			res = c.countArrangements(springTypes[1:], numbers, remaining)
		case 0:
			res = c.countArrangements(springTypes[1:], numbers[1:], -1)
		default:
		}
	case springTypeDamaged:
		switch remaining {
		case -1:
			if len(numbers) == 0 {
				break
			}
			res = c.countArrangements(springTypes[1:], numbers, numbers[0]-1)
		case 0:
		default:
			res = c.countArrangements(springTypes[1:], numbers, remaining-1)
		}
	default:
		panic(springType)
	}

	c.cache[entry] = res
	return res
}

func fs2(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	res := 0
	for scanner.Scan() {
		springTypes, numbers := parse(scanner.Text())
		springTypes, numbers = unfold(springTypes, numbers, 5)
		res += counter{cache: make(map[Entry]int)}.
			countArrangements(springTypes, numbers, -1)
	}

	return res
}

func unfold(springTypes []SpringType, numbers []int, count int) ([]SpringType, []int) {
	var resSpringTypes []SpringType
	var resNumbers []int
	for i := 0; i < count; i++ {
		resSpringTypes = append(resSpringTypes, springTypes...)
		if i != count-1 {
			resSpringTypes = append(resSpringTypes, springTypeUnknown)
		}
		resNumbers = append(resNumbers, numbers...)
	}
	return resSpringTypes, resNumbers
}

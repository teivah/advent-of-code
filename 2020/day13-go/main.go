package main

import (
	"io"
	"sort"

	aoc "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	lines := aoc.ReaderToStrings(input)

	earliest := aoc.StringToInt(lines[0])
	delimiter := aoc.NewDelimiter(lines[1], ",")
	words := delimiter.GetStrings()
	var buses []int
	for _, word := range words {
		n, isInt := aoc.TryStringToInt(word)
		if isInt {
			buses = append(buses, n)
		}
	}

	for i := earliest; ; i++ {
		for _, bus := range buses {
			if i%bus == 0 {
				return (i - earliest) * bus
			}
		}
	}
}

func fs2(input io.Reader) int {
	lines := aoc.ReaderToStrings(input)

	delimiter := aoc.NewDelimiter(lines[1], ",")
	words := delimiter.GetStrings()
	var buses []Bus
	for i, word := range words {
		n, isInt := aoc.TryStringToInt(word)
		if isInt {
			buses = append(buses, Bus{n, i})
		}
	}

	sort.Slice(buses, func(i, j int) bool {
		a := buses[i]
		b := buses[j]
		return b.id < a.id
	})

	i := -buses[0].delay
	for ; ; i += buses[0].id {
		if (i+buses[1].delay)%buses[1].id == 0 {
			break
		}
	}

	lcmList := []int{buses[0].id, buses[1].id}
	for j := 2; j < len(buses); j++ {
		lcm := leastCommonMultiple(lcmList)

		for ; ; i += lcm {
			if (i+buses[j].delay)%buses[j].id == 0 {
				break
			}
		}
		lcmList = append(lcmList, buses[j].id)
	}
	return i
}

type Bus struct {
	id    int
	delay int
}

func gcd(a, b int) int {
	for b != 0 {
		a, b = b, a%b
	}
	return a
}

func leastCommonMultiple(numbers []int) int {
	lcm := numbers[0]
	for _, number := range numbers[1:] {
		lcm = lcm * number / gcd(lcm, number)
	}
	return lcm
}

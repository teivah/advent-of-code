package main

import (
	"io"
	"strings"

	aoc "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	lines := aoc.ReaderToStrings(input)
	programs := toPrograms(lines)

	memory := make(map[uint]uint)
	for _, program := range programs {
		for _, write := range program.writes {
			v := applyMask(write.value, program.mask)
			memory[write.address] = v
		}
	}

	sum := 0
	for _, v := range memory {
		sum += int(v)
	}

	return sum
}

func applyMask(value uint, mask string) uint {
	for i := len(mask) - 1; i >= 0; i-- {
		r := mask[i]
		switch r {
		case '0':
			// Complement
			m := (1 << (len(mask) - i - 1)) ^ ^uint(0)
			value = value & m
		case '1':
			value = value | (1 << (len(mask) - i - 1))
		}
	}
	return value
}

func toPrograms(lines []string) []Program {
	var programs []Program

	for i := 0; i < len(lines); {
		del := aoc.NewDelimiter(lines[i], " ")
		mask := del.GetString(2)

		j := i + 1
		var writes []Write
		for ; j < len(lines); j++ {
			if strings.HasPrefix(lines[j], "mask") {
				break
			}

			del2 := aoc.NewDelimiter(lines[j], " ")
			s := del2.GetString(0)
			from := aoc.StringToInt(s[4 : len(s)-1])
			to := del2.GetInt(2)

			writes = append(writes, Write{
				address: uint(from),
				value:   uint(to),
			})
		}

		programs = append(programs, Program{
			mask:   mask,
			writes: writes,
		})

		i = j
	}
	return programs
}

type Program struct {
	mask   string
	writes []Write
}

type Write struct {
	address uint
	value   uint
}

func fs2(input io.Reader) int {
	lines := aoc.ReaderToStrings(input)
	programs := toPrograms(lines)

	memory := make(map[uint]uint)
	for _, program := range programs {
		for _, write := range program.writes {
			addresses := applyMask2(len(program.mask)-1, write.address, program.mask)
			for _, address := range addresses {
				memory[address] = write.value
			}
		}
	}

	sum := 0
	for _, v := range memory {
		sum += int(v)
	}

	return sum
}

func applyMask2(i int, value uint, mask string) []uint {
	if i == -1 {
		return []uint{value}
	}

	r := mask[i]
	switch r {
	case '0':
		return applyMask2(i-1, value, mask)
	case '1':
		value = value | (1 << (len(mask) - i - 1))
		return applyMask2(i-1, value, mask)
	case 'X':
		// With zero
		m := (1 << (len(mask) - i - 1)) ^ ^uint(0)
		a := applyMask2(i-1, value&m, mask)
		// With one
		b := applyMask2(i-1, value|(1<<(len(mask)-i-1)), mask)
		return append(a, b...)
	default:
		panic(mask)
	}
}

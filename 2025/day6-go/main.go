package main

import (
	"fmt"
	"io"
	"strings"

	"github.com/teivah/go-aoc"
)

func fs1(input io.Reader) int {
	lines := aoc.ReaderToStrings(input)

	var digits [][]int
	for i := 0; i < len(lines)-1; i++ {
		del := aoc.NewDelimiter(lines[i], " ", aoc.WithTrimSpace())
		digits = append(digits, del.GetInts())
	}
	ops := aoc.NewDelimiter(lines[len(lines)-1], " ", aoc.WithTrimSpace()).GetStrings()

	res := 0
	for col := 0; col < len(digits[0]); col++ {
		cur := 0
		for row := 0; row < len(digits); row++ {
			if row == 0 {
				cur = digits[row][col]
				continue
			}
			if ops[col] == "*" {
				cur *= digits[row][col]
			} else {
				cur += digits[row][col]
			}
		}
		res += cur
	}
	return res
}

type Op struct {
	add bool
	idx int
}

func fs2(input io.Reader) int {
	lines := aoc.ReaderToStrings(input)
	// Stupid IDE trimming my lines.
	maxCols := 0
	for _, line := range lines {
		maxCols = max(maxCols, len(line))
	}
	for i, line := range lines {
		runes := []rune(line)
		for len(runes) != maxCols {
			runes = append(runes, ' ')
		}
		lines[i] = string(runes)
	}
	var ops []Op
	last := lines[len(lines)-1]
	for i := 0; i < len(last); i++ {
		switch last[i] {
		case ' ':
			continue
		case '*':
			ops = append(ops, Op{false, i})
		case '+':
			ops = append(ops, Op{true, i})
		default:
			panic(last[i])
		}
	}

	res := 0
	for i := 0; i < len(ops); i++ {
		from := ops[i].idx
		to := 0
		if i == len(ops)-1 {
			to = maxCols - 1
		} else {
			to = ops[i+1].idx - 1
		}

		var values []int
		for col := from; col <= to; col++ {
			var cur []int
			for j := 0; j < len(lines)-1; j++ {
				r := rune(lines[j][col])
				if r == ' ' {
					continue
				}
				v := aoc.RuneToInt(r)
				cur = append(cur, v)
			}
			if len(cur) == 0 {
				continue
			}
			v := intsToInt(cur)
			values = append(values, v)
		}

		cur := values[0]
		for j := 1; j < len(values); j++ {
			if ops[i].add {
				cur += values[j]
			} else {
				cur *= values[j]
			}
		}
		res += cur
	}
	return res
}

func intsToInt(v []int) int {
	sb := strings.Builder{}
	for _, n := range v {
		sb.WriteString(fmt.Sprintf("%d", n))
	}
	return aoc.StringToInt(sb.String())
}

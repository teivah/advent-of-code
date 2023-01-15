package main

import (
	"bufio"
	"io"
	"strconv"
	"strings"
)

func fs1(input io.Reader, clear string) string {
	scanner := bufio.NewScanner(input)
	var instructions []Instruction
	for scanner.Scan() {
		s := scanner.Text()
		idx := indexAll(s, " ")
		if strings.Index(s, "swap position") != -1 {
			instructions = append(instructions, SwapPosition{
				x: toint(s[idx[1]+1 : idx[2]]),
				y: toint(s[idx[4]+1:]),
			})
		} else if strings.Index(s, "swap letter") != -1 {
			instructions = append(instructions, SwapLetter{
				x: rune(s[idx[1]+1]),
				y: rune(s[idx[4]+1]),
			})
		} else if strings.Index(s, "rotate based") != -1 {
			instructions = append(instructions, RotatePosition{
				x: rune(s[idx[5]+1]),
			})
		} else if strings.Index(s, "rotate") != -1 {
			left := strings.Index(s, "rotate left") != -1
			step := toint(s[idx[1]+1 : idx[2]])
			if left {
				step = -step
			}
			instructions = append(instructions, Rotate{
				step: step,
			})
		} else if strings.Index(s, "reverse") != -1 {
			instructions = append(instructions, ReversePosition{
				from: toint(s[idx[1]+1 : idx[2]]),
				to:   toint(s[idx[3]+1:]),
			})
		} else if strings.Index(s, "move") != -1 {
			instructions = append(instructions, Move{
				x: toint(s[idx[1]+1 : idx[2]]),
				y: toint(s[idx[4]+1:]),
			})
		} else {
			panic(s)
		}
	}

	password := []rune(clear)
	for _, instruction := range instructions {
		instruction.apply(password)
	}
	return string(password)
}

func toint(s string) int {
	i, err := strconv.Atoi(s)
	if err != nil {
		panic(s)
	}
	return i
}

type Instruction interface {
	apply(s []rune)
}

type SwapPosition struct {
	x int
	y int
}

func (i SwapPosition) apply(s []rune) {
	s[i.x], s[i.y] = s[i.y], s[i.x]
}

type SwapLetter struct {
	x rune
	y rune
}

func (i SwapLetter) apply(s []rune) {
	x := -1
	y := -1

	for idx := 0; idx < len(s); idx++ {
		r := s[idx]
		if r == i.x {
			x = idx
		}
		if r == i.y {
			y = idx
		}

		if x != -1 && y != -1 {
			break
		}
	}

	s[x], s[y] = s[y], s[x]
}

type Rotate struct {
	step int
}

func (i Rotate) apply(s []rune) {
	rotate(s, i.step)
}

func rotate(s []rune, steps int) {
	res := make([]rune, len(s))
	for idx := 0; idx < len(s); idx++ {
		v := mod(idx-steps, len(s))
		res[idx] = s[v]
	}
	copy(s, res)
}

type RotatePosition struct {
	x rune
}

func (i RotatePosition) apply(s []rune) {
	idx := -1
	for n, r := range s {
		if r == i.x {
			idx = n
			break
		}
	}

	rotations := 1 + idx
	if idx >= 4 {
		rotations++
	}

	rotate(s, rotations)
}

type ReversePosition struct {
	from int
	to   int
}

func (i ReversePosition) apply(s []rune) {
	x := i.from
	y := i.to
	for x < y {
		s[x], s[y] = s[y], s[x]
		x++
		y--
	}
}

type Move struct {
	x int
	y int
}

func (i Move) apply(s []rune) {
	if i.x == i.y {
		return
	}

	r := s[i.x]

	v := append(s[:i.x], s[i.x+1:]...)

	var res []rune
	if i.y == len(s)-1 {
		res = append(v[:i.y], r)
	} else {
		res = make([]rune, 0, len(s))
		added := false
		for idx := 0; idx < len(s); idx++ {
			if idx == i.y {
				res = append(res, r)
				added = true
			} else {
				if added {
					res = append(res, v[idx-1])
				} else {
					res = append(res, v[idx])
				}
			}
		}
		if !added {
			res = append(res, r)
		}
	}
	copy(s, res)
}

func fs2(input io.Reader, target string) string {
	scanner := bufio.NewScanner(input)
	var instructions []Instruction
	for scanner.Scan() {
		s := scanner.Text()
		idx := indexAll(s, " ")
		if strings.Index(s, "swap position") != -1 {
			instructions = append(instructions, SwapPosition{
				x: toint(s[idx[1]+1 : idx[2]]),
				y: toint(s[idx[4]+1:]),
			})
		} else if strings.Index(s, "swap letter") != -1 {
			instructions = append(instructions, SwapLetter{
				x: rune(s[idx[1]+1]),
				y: rune(s[idx[4]+1]),
			})
		} else if strings.Index(s, "rotate based") != -1 {
			instructions = append(instructions, RotatePosition{
				x: rune(s[idx[5]+1]),
			})
		} else if strings.Index(s, "rotate") != -1 {
			left := strings.Index(s, "rotate left") != -1
			step := toint(s[idx[1]+1 : idx[2]])
			if left {
				step = -step
			}
			instructions = append(instructions, Rotate{
				step: step,
			})
		} else if strings.Index(s, "reverse") != -1 {
			instructions = append(instructions, ReversePosition{
				from: toint(s[idx[1]+1 : idx[2]]),
				to:   toint(s[idx[3]+1:]),
			})
		} else if strings.Index(s, "move") != -1 {
			instructions = append(instructions, Move{
				x: toint(s[idx[1]+1 : idx[2]]),
				y: toint(s[idx[4]+1:]),
			})
		} else {
			panic(s)
		}
	}

	perms := permutations(0, []rune(target))
	for _, perm := range perms {
		if password(instructions, perm) == target {
			return perm
		}
	}

	return ""
}

func permutations(idx int, runes []rune) []string {
	if idx == len(runes) {
		return []string{string(runes)}
	}

	var res []string
	for i := idx; i < len(runes); i++ {
		runes[i], runes[idx] = runes[idx], runes[i]
		res = append(res, permutations(idx+1, runes)...)
		runes[i], runes[idx] = runes[idx], runes[i]
	}
	return res
}

func password(instructions []Instruction, clear string) string {
	password := []rune(clear)
	for _, instruction := range instructions {
		instruction.apply(password)
	}
	return string(password)
}

func indexAll(s string, search string) []int {
	i := 0
	var res []int
	for i < len(s) {
		index := strings.Index(s[i:], search)
		if index == -1 {
			return res
		}
		res = append(res, index+i)
		i += index + len(search)
	}
	return res
}

func mod(d, m int) int {
	res := d % m
	if (res < 0 && m > 0) || (res > 0 && m < 0) {
		return res + m
	}
	return res
}

package main

import (
	"bufio"
	"io"
	"strconv"
	"strings"
)

func fs1(input io.Reader) (int, error) {
	scanner := bufio.NewScanner(input)
	var instructions []Instruction
	for scanner.Scan() {
		s := scanner.Text()
		if strings.Contains(s, "hlf") {
			instructions = append(instructions, Hlf{s[4:]})
		} else if strings.Contains(s, "tpl") {
			instructions = append(instructions, Tpl{s[4:]})
		} else if strings.Contains(s, "inc") {
			instructions = append(instructions, Inc{s[4:]})
		} else if strings.Contains(s, "jmp") {
			jmp, err := strconv.Atoi(s[4:])
			if err != nil {
				return 0, err
			}
			instructions = append(instructions, Jmp{jmp})
		} else if strings.Contains(s, "jie") {
			idx := indexAll(s, " ")
			reg := s[idx[0]+1 : idx[1]-1]
			offset, err := strconv.Atoi(s[idx[1]+1:])
			if err != nil {
				return 0, err
			}
			instructions = append(instructions, Jie{reg, offset})
		} else if strings.Contains(s, "jio") {
			idx := indexAll(s, " ")
			reg := s[idx[0]+1 : idx[1]-1]
			offset, err := strconv.Atoi(s[idx[1]+1:])
			if err != nil {
				return 0, err
			}
			instructions = append(instructions, Jio{reg, offset})
		} else {
			panic(s)
		}
	}

	i := 0
	a := 0
	b := 0
	for i < len(instructions) {
		na, nb, offset := instructions[i].apply(a, b)
		a = na
		b = nb
		i += offset
	}

	return b, nil
}

type Instruction interface {
	apply(a int, b int) (int, int, int)
}

type Hlf struct {
	reg string
}

func (h Hlf) apply(a int, b int) (int, int, int) {
	if h.reg == "a" {
		return a / 2, b, 1
	}
	return a, b / 2, 1
}

type Tpl struct {
	reg string
}

func (t Tpl) apply(a int, b int) (int, int, int) {
	if t.reg == "a" {
		return a * 3, b, 1
	}
	return a, b * 3, 1
}

type Inc struct {
	reg string
}

func (i Inc) apply(a int, b int) (int, int, int) {
	if i.reg == "a" {
		return a + 1, b, 1
	}
	return a, b + 1, 1
}

type Jmp struct {
	offset int
}

func (j Jmp) apply(a int, b int) (int, int, int) {
	return a, b, j.offset
}

type Jie struct {
	reg    string
	offset int
}

func (j Jie) apply(a int, b int) (int, int, int) {
	if j.reg == "a" {
		if a%2 == 0 {
			return a, b, j.offset
		}
		return a, b, 1
	}
	if b%2 == 0 {
		return a, b, j.offset
	}
	return a, b, 1
}

type Jio struct {
	reg    string
	offset int
}

func (j Jio) apply(a int, b int) (int, int, int) {
	if j.reg == "a" {
		if a == 1 {
			return a, b, j.offset
		}
		return a, b, 1
	}
	if b == 1 {
		return a, b, j.offset
	}
	return a, b, 1
}

func fs2(input io.Reader) (int, error) {
	scanner := bufio.NewScanner(input)
	var instructions []Instruction
	for scanner.Scan() {
		s := scanner.Text()
		if strings.Contains(s, "hlf") {
			instructions = append(instructions, Hlf{s[4:]})
		} else if strings.Contains(s, "tpl") {
			instructions = append(instructions, Tpl{s[4:]})
		} else if strings.Contains(s, "inc") {
			instructions = append(instructions, Inc{s[4:]})
		} else if strings.Contains(s, "jmp") {
			jmp, err := strconv.Atoi(s[4:])
			if err != nil {
				return 0, err
			}
			instructions = append(instructions, Jmp{jmp})
		} else if strings.Contains(s, "jie") {
			idx := indexAll(s, " ")
			reg := s[idx[0]+1 : idx[1]-1]
			offset, err := strconv.Atoi(s[idx[1]+1:])
			if err != nil {
				return 0, err
			}
			instructions = append(instructions, Jie{reg, offset})
		} else if strings.Contains(s, "jio") {
			idx := indexAll(s, " ")
			reg := s[idx[0]+1 : idx[1]-1]
			offset, err := strconv.Atoi(s[idx[1]+1:])
			if err != nil {
				return 0, err
			}
			instructions = append(instructions, Jio{reg, offset})
		} else {
			panic(s)
		}
	}

	i := 0
	a := 1
	b := 0
	for i < len(instructions) {
		na, nb, offset := instructions[i].apply(a, b)
		a = na
		b = nb
		i += offset
	}

	return b, nil
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

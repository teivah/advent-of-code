package main

import (
	"bufio"
	"io"
	"strconv"
	"strings"
)

func fs(input io.Reader, reg map[string]int) (int, error) {
	scanner := bufio.NewScanner(input)
	var instructions []Instruction
	for scanner.Scan() {
		s := scanner.Text()
		idx := indexAll(s, " ")
		if strings.Index(s, "cpy") != -1 {
			s2 := s[idx[1]+1:]
			s1 := s[idx[0]+1 : idx[1]]
			i1, err := strconv.Atoi(s1)
			if err != nil {
				instructions = append(instructions, Cpy{
					vr1: ValueReg{r: &s1},
					r2:  s2,
				})
			} else {
				instructions = append(instructions, Cpy{
					vr1: ValueReg{i: &i1},
					r2:  s2,
				})
			}
		} else if strings.Index(s, "inc") != -1 {
			instructions = append(instructions, Inc{s[4:]})
		} else if strings.Index(s, "dec") != -1 {
			instructions = append(instructions, Dec{s[4:]})
		} else if strings.Index(s, "jnz") != -1 {
			s2 := s[idx[1]+1:]
			i2, err := strconv.Atoi(s2)
			if err != nil {
				panic(err)
			}

			s1 := s[idx[0]+1 : idx[1]]
			i1, err := strconv.Atoi(s1)
			if err != nil {
				instructions = append(instructions, Jnz{
					vr1:  ValueReg{r: &s1},
					jump: i2,
				})
			} else {
				instructions = append(instructions, Jnz{
					vr1:  ValueReg{i: &i1},
					jump: i2,
				})
			}
		} else {
			panic(s)
		}
	}

	for i := 0; i < len(instructions); {
		i += instructions[i].apply(reg)
	}

	return reg["a"], nil
}

type Instruction interface {
	apply(reg map[string]int) int
}

type Cpy struct {
	vr1 ValueReg
	r2  string
}

func (c Cpy) apply(reg map[string]int) int {
	if c.vr1.r != nil {
		reg[c.r2] = reg[*c.vr1.r]
	} else {
		reg[c.r2] = *c.vr1.i
	}
	return 1
}

type Inc struct {
	r string
}

func (i Inc) apply(reg map[string]int) int {
	reg[i.r]++
	return 1
}

type Dec struct {
	r string
}

func (d Dec) apply(reg map[string]int) int {
	reg[d.r]--
	return 1
}

type Jnz struct {
	vr1  ValueReg
	jump int
}

func (j Jnz) apply(reg map[string]int) int {
	v := 0
	if j.vr1.r != nil {
		v = reg[*j.vr1.r]
	} else {
		v = *j.vr1.i
	}

	if v != 0 {
		return j.jump
	}
	return 1
}

type ValueReg struct {
	r *string
	i *int
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

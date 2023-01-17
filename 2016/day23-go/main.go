package main

import (
	"bufio"
	"io"
	"strconv"
	"strings"
)

func fs1(input io.Reader, reg map[string]int16) (int16, error) {
	scanner := bufio.NewScanner(input)
	var instructions []Instruction
	for scanner.Scan() {
		s := scanner.Text()
		idx := indexAll(s, " ")
		if strings.Index(s, "cpy") != -1 {
			s2 := s[idx[1]+1:]
			s1 := s[idx[0]+1 : idx[1]]
			i1, err := strconv.Atoi(s1)
			t1 := int16(i1)
			if err != nil {
				instructions = append(instructions, Cpy{
					vr1: ValueReg{r: &s1},
					r2:  s2,
				})
			} else {
				instructions = append(instructions, Cpy{
					vr1: ValueReg{i: &t1},
					r2:  s2,
				})
			}
		} else if strings.Index(s, "inc") != -1 {
			instructions = append(instructions, Inc{s[4:]})
		} else if strings.Index(s, "dec") != -1 {
			instructions = append(instructions, Dec{s[4:]})
		} else if strings.Index(s, "tgl") != -1 {
			instructions = append(instructions, &Tgl{s[4:], false})
		} else if strings.Index(s, "jnz") != -1 {
			s2 := s[idx[1]+1:]
			i2, err := strconv.Atoi(s2)
			var jump ValueReg
			if err != nil {
				jump = ValueReg{r: &s2}
			} else {
				t1 := int16(i2)
				jump = ValueReg{i: &t1}
			}

			s1 := s[idx[0]+1 : idx[1]]
			i1, err := strconv.Atoi(s1)
			if err != nil {
				instructions = append(instructions, Jnz{
					vr1:  ValueReg{r: &s1},
					jump: jump,
				})
			} else {
				t1 := int16(i1)
				instructions = append(instructions, Jnz{
					vr1:  ValueReg{i: &t1},
					jump: jump,
				})
			}
		} else {
			panic(s)
		}
	}

	root := make([]Instruction, len(instructions))
	copy(root, instructions)

	var offset int16
	for int(offset) < len(instructions) {
		offset += instructions[offset].apply(offset, instructions, reg)
	}

	return reg["a"], nil
}

type Instruction interface {
	apply(offset int16, instructions []Instruction, reg map[string]int16) int16
	toggle() Instruction
}

type Cpy struct {
	vr1 ValueReg
	r2  string
}

func (c Cpy) apply(offset int16, instructions []Instruction, reg map[string]int16) int16 {
	if c.vr1.r != nil {
		reg[c.r2] = reg[*c.vr1.r]
	} else {
		reg[c.r2] = *c.vr1.i
	}
	return 1
}

func (c Cpy) toggle() Instruction {
	return Jnz{
		vr1:  c.vr1,
		jump: ValueReg{r: &c.r2},
	}
}

type Inc struct {
	r string
}

func (i Inc) apply(offset int16, instructions []Instruction, reg map[string]int16) int16 {
	reg[i.r]++
	return 1
}

func (i Inc) toggle() Instruction {
	return Dec{
		r: i.r,
	}
}

type Dec struct {
	r string
}

func (d Dec) apply(offset int16, instructions []Instruction, reg map[string]int16) int16 {
	reg[d.r]--
	return 1
}

func (d Dec) toggle() Instruction {
	return Inc{
		r: d.r,
	}
}

type Mult struct {
	r string
}

func (d Mult) apply(offset int16, instructions []Instruction, reg map[string]int16) int16 {
	reg[d.r] *= 2
	return 1
}

func (d Mult) toggle() Instruction {
	return Inc{
		r: d.r,
	}
}

type Tgl struct {
	r    string
	skip bool
}

func (t *Tgl) apply(offset int16, instructions []Instruction, reg map[string]int16) int16 {
	if t.skip {
		t.skip = false
		return 1
	}

	if int(offset+reg[t.r]) >= len(instructions) {
		return 1
	}

	instructions[offset+reg[t.r]] = instructions[offset+reg[t.r]].toggle()
	return 1
}

func (t *Tgl) toggle() Instruction {
	return Inc{
		r: t.r,
	}
}

type Jnz struct {
	vr1  ValueReg
	jump ValueReg
}

func (j Jnz) apply(offset int16, instructions []Instruction, reg map[string]int16) int16 {
	var v int16
	if j.vr1.r != nil {
		v = reg[*j.vr1.r]
	} else {
		v = *j.vr1.i
	}

	if v != 0 {
		if j.jump.i != nil {
			return *j.jump.i
		}
		return reg[*j.jump.r]
	}
	return 1
}

func (j Jnz) toggle() Instruction {
	if j.jump.i != nil {
		return Noop{}
	}

	return Cpy{
		vr1: j.vr1,
		r2:  *j.jump.r,
	}
}

type Noop struct{}

func (n Noop) apply(offset int16, instructions []Instruction, reg map[string]int16) int16 {
	return 1
}

func (n Noop) toggle() Instruction {
	panic("togle noop")
}

type ValueReg struct {
	r *string
	i *int16
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

func fs2(input io.Reader, reg map[string]int16) (int16, error) {
	scanner := bufio.NewScanner(input)
	var instructions []Instruction
	for scanner.Scan() {
		s := scanner.Text()
		idx := indexAll(s, " ")
		if strings.Index(s, "cpy") != -1 {
			s2 := s[idx[1]+1:]
			s1 := s[idx[0]+1 : idx[1]]
			i1, err := strconv.Atoi(s1)
			t1 := int16(i1)
			if err != nil {
				instructions = append(instructions, Cpy{
					vr1: ValueReg{r: &s1},
					r2:  s2,
				})
			} else {
				instructions = append(instructions, Cpy{
					vr1: ValueReg{i: &t1},
					r2:  s2,
				})
			}
		} else if strings.Index(s, "inc") != -1 {
			instructions = append(instructions, Mult{s[4:]})
		} else if strings.Index(s, "dec") != -1 {
			instructions = append(instructions, Dec{s[4:]})
		} else if strings.Index(s, "tgl") != -1 {
			instructions = append(instructions, &Tgl{s[4:], false})
		} else if strings.Index(s, "jnz") != -1 {
			s2 := s[idx[1]+1:]
			i2, err := strconv.Atoi(s2)
			var jump ValueReg
			if err != nil {
				jump = ValueReg{r: &s2}
			} else {
				t1 := int16(i2)
				jump = ValueReg{i: &t1}
			}

			s1 := s[idx[0]+1 : idx[1]]
			i1, err := strconv.Atoi(s1)
			if err != nil {
				instructions = append(instructions, Jnz{
					vr1:  ValueReg{r: &s1},
					jump: jump,
				})
			} else {
				t1 := int16(i1)
				instructions = append(instructions, Jnz{
					vr1:  ValueReg{i: &t1},
					jump: jump,
				})
			}
		} else {
			panic(s)
		}
	}

	root := make([]Instruction, len(instructions))
	copy(root, instructions)

	var offset int16
	for int(offset) < len(instructions) {
		offset += instructions[offset].apply(offset, instructions, reg)
	}

	return reg["a"], nil
}

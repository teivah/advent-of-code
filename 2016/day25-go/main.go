package main

import (
	"bufio"
	"errors"
	"fmt"
	"io"
	"math"
	"strconv"
	"strings"
)

const outInitValue = 1

func fs1(input io.Reader) (int32, error) {
	scanner := bufio.NewScanner(input)
	var instructions []Instruction
	var out *Out
	for scanner.Scan() {
		s := scanner.Text()
		idx := indexAll(s, " ")
		if strings.Index(s, "cpy") != -1 {
			s2 := s[idx[1]+1:]
			s1 := s[idx[0]+1 : idx[1]]
			i1, err := strconv.Atoi(s1)
			t1 := int32(i1)
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
		} else if strings.Index(s, "out") != -1 {
			v := s[4:]
			i, err := strconv.Atoi(v)
			if err != nil {
				out = &Out{ValueReg{r: &v}, outInitValue, 0}
				instructions = append(instructions, out)
			} else {
				t := int32(i)
				out = &Out{ValueReg{i: &t}, outInitValue, 0}
				instructions = append(instructions, out)
			}
		} else if strings.Index(s, "mul") != -1 {
			r := s[idx[2]+1:]

			s1 := s[idx[0]+1 : idx[1]]
			i1, err := strconv.Atoi(s1)
			var vr1 ValueReg
			if err != nil {
				vr1 = ValueReg{r: &s1}
			} else {
				t1 := int32(i1)
				vr1 = ValueReg{i: &t1}
			}

			s2 := s[idx[1]+1 : idx[2]]
			i2, err := strconv.Atoi(s2)
			var vr2 ValueReg
			if err != nil {
				vr2 = ValueReg{r: &s2}
			} else {
				t2 := int32(i2)
				vr2 = ValueReg{i: &t2}
			}
			instructions = append(instructions, Mult{vr1, vr2, r})
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
				t1 := int32(i2)
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
				t1 := int32(i1)
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

	for i := 0; i < math.MaxInt32; i++ {
		fmt.Println(i)
		var offset int32
		copy(instructions, root)
		reg := map[string]int32{"a": int32(i)}
		out.previous = outInitValue
		out.count = 0
		for int(offset) < len(instructions) {
			v, err := instructions[offset].apply(offset, instructions, reg)
			if err != nil {
				if err == errOver {
					return int32(i), nil
				}
				break
			}
			offset += v
		}
	}

	return 0, nil
}

type Instruction interface {
	apply(offset int32, instructions []Instruction, reg map[string]int32) (int32, error)
	toggle() Instruction
}

type Cpy struct {
	vr1 ValueReg
	r2  string
}

func (c Cpy) apply(offset int32, instructions []Instruction, reg map[string]int32) (int32, error) {
	if c.vr1.r != nil {
		reg[c.r2] = reg[*c.vr1.r]
	} else {
		reg[c.r2] = *c.vr1.i
	}
	return 1, nil
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

func (i Inc) apply(offset int32, instructions []Instruction, reg map[string]int32) (int32, error) {
	reg[i.r]++
	return 1, nil
}

func (i Inc) toggle() Instruction {
	return Dec{
		r: i.r,
	}
}

type Dec struct {
	r string
}

func (d Dec) apply(offset int32, instructions []Instruction, reg map[string]int32) (int32, error) {
	reg[d.r]--
	return 1, nil
}

func (d Dec) toggle() Instruction {
	return Inc{
		r: d.r,
	}
}

type Mult struct {
	r1 ValueReg
	r2 ValueReg
	r  string
}

func (d Mult) apply(offset int32, instructions []Instruction, reg map[string]int32) (int32, error) {
	var v1 int32
	if d.r1.r != nil {
		v1 = reg[*d.r1.r]
	} else {
		v1 = *d.r1.i
	}

	var v2 int32
	if d.r2.r != nil {
		v2 = reg[*d.r2.r]
	} else {
		v2 = *d.r2.i
	}

	reg[d.r] = v1 * v2
	return 1, nil
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

func (t *Tgl) apply(offset int32, instructions []Instruction, reg map[string]int32) (int32, error) {
	if t.skip {
		t.skip = false
		return 1, nil
	}

	if int(offset+reg[t.r]) >= len(instructions) {
		return 1, nil
	}

	instructions[offset+reg[t.r]] = instructions[offset+reg[t.r]].toggle()
	return 1, nil
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

func (j Jnz) apply(offset int32, instructions []Instruction, reg map[string]int32) (int32, error) {
	var v int32
	if j.vr1.r != nil {
		v = reg[*j.vr1.r]
	} else {
		v = *j.vr1.i
	}

	if v != 0 {
		if j.jump.i != nil {
			return *j.jump.i, nil
		}
		return reg[*j.jump.r], nil
	}
	return 1, nil
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

func (n Noop) apply(offset int32, instructions []Instruction, reg map[string]int32) (int32, error) {
	return 1, nil
}

func (n Noop) toggle() Instruction {
	panic("toggle noop")
}

var errOver = errors.New("over")

type Out struct {
	r        ValueReg
	previous int32
	count    int
}

func (o *Out) apply(_ int32, _ []Instruction, reg map[string]int32) (int32, error) {
	v := reg[*o.r.r]
	fmt.Println(v)
	expected := 1 - o.previous
	if v != expected {
		return 0, errors.New("unexpected signal")
	}

	o.count++
	if o.count > 100 {
		return 0, errOver
	}

	o.previous = expected
	return 1, nil
}

func (o *Out) toggle() Instruction {
	panic("toggle out")
}

type ValueReg struct {
	r *string
	i *int32
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

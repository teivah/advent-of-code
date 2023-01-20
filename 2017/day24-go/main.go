package main

import (
	"bufio"
	"io"

	lib "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	ports := make(map[int][]int)
	for scanner.Scan() {
		line := scanner.Text()
		del := lib.NewDelimiter(line, "/")

		in := del.GetInt(0)
		out := del.GetInt(1)
		ports[in] = append(ports[in], out)
		ports[out] = append(ports[out], in)
	}

	p := &Ports{ports}
	return best(p, 0, 0)
}

func copy(s []int) []int {
	res := make([]int, len(s))
	for i := 0; i < len(s); i++ {
		res[i] = s[i]
	}
	return res
}

func best(p *Ports, lastOut int, cur int) int {
	v, exists := p.ports[lastOut]
	if !exists {
		return cur
	}

	outs := copy(v)
	max := cur
	for _, out := range outs {
		a := p.remove(lastOut, out)
		b := p.remove(out, lastOut)
		max = lib.Max(max, best(p, out, cur+lastOut+out))
		b()
		a()
	}
	return max
}

type Ports struct {
	ports map[int][]int
}

func (p *Ports) remove(in, out int) func() {
	if len(p.ports[in]) == 1 {
		delete(p.ports, in)
		return func() {
			p.ports[in] = []int{out}
		}
	}

	res := make([]int, 0, len(p.ports[in])-1)

	i := 0
	s := p.ports[in]
	for ; ; i++ {
		if s[i] != out {
			res = append(res, s[i])
			continue
		}
		i++
		break
	}
	for ; i < len(s); i++ {
		res = append(res, s[i])
	}

	p.ports[in] = res
	return func() {
		p.ports[in] = append(p.ports[in], out)
	}
}

func fs2(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	ports := make(map[int][]int)
	for scanner.Scan() {
		line := scanner.Text()
		del := lib.NewDelimiter(line, "/")

		in := del.GetInt(0)
		out := del.GetInt(1)
		ports[in] = append(ports[in], out)
		ports[out] = append(ports[out], in)
	}

	p := &Ports{ports}
	_, strength := strengthLongest(p, 0, 0, 0)
	return strength
}

func strengthLongest(p *Ports, lastOut int, curLength, curStrength int) (int, int) {
	v, exists := p.ports[lastOut]
	if !exists {
		return curLength, curStrength
	}

	outs := copy(v)
	maxLength := 0
	maxStrength := 0
	for _, out := range outs {
		a := p.remove(lastOut, out)
		b := p.remove(out, lastOut)
		l, s := strengthLongest(p, out, curLength+2, curStrength+lastOut+out)
		if l > maxLength {
			maxLength = l
			maxStrength = s
		} else if l == maxLength {
			if s > maxStrength {
				maxLength = l
				maxStrength = s
			}
		}
		b()
		a()
	}
	return maxLength, maxStrength
}

package main

import "fmt"

func fs1(nbElements int, lengths []int) int {
	s := make([]int, nbElements)
	for i := 0; i < nbElements; i++ {
		s[i] = i
	}

	i := 0
	skip := 0
	for _, length := range lengths {
		reverse(s, i, length)
		i = mod(i+length+skip, nbElements)
		skip++
	}

	return s[0] * s[1]
}

func toSequence(s string) []int {
	var res []int
	for i := 0; i < len(s); i++ {
		r := rune(s[i])
		res = append(res, int(r))
	}
	res = append(res, []int{17, 31, 73, 47, 23}...)
	return res
}

func fs2(nbElements int, s string) string {
	knots := make([]int, nbElements)
	for i := 0; i < nbElements; i++ {
		knots[i] = i
	}

	seq := toSequence(s)

	i := 0
	skip := 0
	for round := 0; round < 64; round++ {
		for _, length := range seq {
			reverse(knots, i, length)
			i = i + length + skip%nbElements
			skip++
		}
	}

	return knotHash(denseHash(knots))
}

func reverse(s []int, i, length int) {
	if length == 1 {
		return
	}

	start := i % len(s)
	end := mod(i+length-1, len(s))
	for j := 0; j < length/2; j++ {
		s[start], s[end] = s[end], s[start]
		start = mod(start+1, len(s))
		end = mod(end-1, len(s))
	}
}

func denseHash(seq []int) []int {
	res := make([]int, 0, 16)
	for i := 0; i < len(seq); i += 16 {
		v := 0
		for j := 0; j < 16; j++ {
			v ^= seq[i+j]
		}
		res = append(res, v)
	}
	return res
}

func knotHash(seq []int) string {
	res := ""
	for _, v := range seq {
		x := fmt.Sprintf("%x", v)
		if len(x) == 1 {
			x = "0" + x
		}
		res += x
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

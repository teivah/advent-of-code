package main

import (
	"fmt"
	"strconv"
)

func fs1(key string) int {
	sum := 0
	for i := 0; i < 128; i++ {
		h := getKnotHash(key + "-" + strconv.Itoa(i))
		binary := hexToBinary(h)
		sum += count1s(binary)
	}

	return sum
}

func count1s(s string) int {
	sum := 0
	for i := 0; i < len(s); i++ {
		if s[i] == '1' {
			sum++
		}
	}
	return sum
}

func hexToBinary(s string) string {
	output := ""
	for _, c := range s {
		switch c {
		case '0':
			output += "0000"
		case '1':
			output += "0001"
		case '2':
			output += "0010"
		case '3':
			output += "0011"
		case '4':
			output += "0100"
		case '5':
			output += "0101"
		case '6':
			output += "0110"
		case '7':
			output += "0111"
		case '8':
			output += "1000"
		case '9':
			output += "1001"
		case 'a':
			output += "1010"
		case 'b':
			output += "1011"
		case 'c':
			output += "1100"
		case 'd':
			output += "1101"
		case 'e':
			output += "1110"
		case 'f':
			output += "1111"
		}
	}
	return output
}

func fs2(key string) int {
	grid := make([][]rune, 0, 128)

	for i := 0; i < 128; i++ {
		h := getKnotHash(key + "-" + strconv.Itoa(i))
		binary := hexToBinary(h)
		grid = append(grid, []rune(binary))
	}

	sum := 0
	visited := make(map[Position]struct{})
	for row := 0; row < 128; row++ {
		for col := 0; col < 128; col++ {
			sum += visit(grid, Position{row, col}, visited)
		}
	}

	return sum
}

type Position struct {
	row int
	col int
}

func (p Position) isValid() bool {
	if p.row < 0 || p.col < 0 || p.row > 127 || p.col > 127 {
		return false
	}
	return true
}

func (p Position) delta(row, col int) Position {
	p.row += row
	p.col += col
	return p
}

func visit(grid [][]rune, pos Position, visited map[Position]struct{}) int {
	if !pos.isValid() {
		return 0
	}

	if _, exists := visited[pos]; exists {
		return 0
	}

	if grid[pos.row][pos.col] == '0' {
		return 0
	}

	visited[pos] = struct{}{}
	visit(grid, pos.delta(-1, 0), visited)
	visit(grid, pos.delta(0, -1), visited)
	visit(grid, pos.delta(0, 1), visited)
	visit(grid, pos.delta(1, 0), visited)

	return 1
}

func getKnotHash(s string) string {
	nbElements := 256
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

	return hash(denseHash(knots))
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

func hash(seq []int) string {
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

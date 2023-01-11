package main

import (
	"bufio"
	"io"
	"strconv"
)

func fs1(input io.Reader) (int, error) {
	scanner := bufio.NewScanner(input)
	var instructions []string
	for scanner.Scan() {
		instructions = append(instructions, scanner.Text())
	}

	x := 0
	y := 0
	res := 0
	for _, instruction := range instructions {
		runes := []rune(instruction)
		for _, r := range runes {
			switch r {
			case 'U':
				y = move(y - 1)
			case 'D':
				y = move(y + 1)
			case 'L':
				x = move(x - 1)
			case 'R':
				x = move(x + 1)
			}
		}
		res = res*10 + getButton(x, y)
	}

	return res, nil
}

func move(v int) int {
	if v == -2 {
		return -1
	}
	if v == 2 {
		return 1
	}
	return v
}

func getButton(x, y int) int {
	return (y+1)*3 + (x + 1) + 1
}

func fs2(input io.Reader) (string, error) {
	scanner := bufio.NewScanner(input)
	var instructions []string
	for scanner.Scan() {
		instructions = append(instructions, scanner.Text())
	}

	p := &Position{x: -2, y: 0}
	res := ""
	for _, instruction := range instructions {
		runes := []rune(instruction)
		for _, r := range runes {
			switch r {
			case 'U':
				p.move(0, -1)
			case 'D':
				p.move(0, 1)
			case 'L':
				p.move(-1, 0)
			case 'R':
				p.move(1, 0)
			}
		}
		res += getButton2(p)
	}

	return res, nil
}

type Position struct {
	x int
	y int
}

func (p *Position) move(dx, dy int) {
	x := p.x + dx
	y := p.y + dy

	if y == -2 || y == 2 {
		if x != 0 {
			return
		}
	} else if y == -1 || y == 1 {
		if x < -1 || x > 1 {
			return
		}
	} else if y == 0 {
		if x < -2 || x > 2 {
			return
		}
	} else {
		return
	}

	p.x = x
	p.y = y
}

func getButton2(pos *Position) string {
	x := pos.x
	y := pos.y

	if y == -2 {
		return "1"
	}
	if y == -1 {
		v := x + 3
		return strconv.Itoa(v)
	}
	if y == 0 {
		v := x + 7
		return strconv.Itoa(v)
	}
	if y == 1 {
		switch x {
		case -1:
			return "A"
		case 0:
			return "B"
		case 1:
			return "C"
		}
	}
	return "D"
}

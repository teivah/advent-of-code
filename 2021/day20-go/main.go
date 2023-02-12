package main

import (
	"io"

	aoc "github.com/teivah/advent-of-code"
)

func fs(input io.Reader, repeat int) int {
	groups := aoc.StringGroups(aoc.ReaderToStrings(input))
	im := toImage(groups)
	for i := 0; i < repeat; i++ {
		im.enhance(i)
	}

	return im.count()
}

type Image struct {
	enhancement string
	lights      map[aoc.Position]bool
	minRow      int
	minCol      int
	maxRow      int
	maxCol      int
}

func (im *Image) String() string {
	s := ""
	for row := im.minRow; row <= im.maxRow; row++ {
		for col := im.minCol; col <= im.maxCol; col++ {
			if im.lights[aoc.Position{row, col}] {
				s += "#"
			} else {
				s += "."
			}
		}
		s += "\n"
	}
	return s
}

func (im *Image) get(round int, pos aoc.Position, deltarow, deltacol int) int {
	k := pos.Delta(deltarow, deltacol)

	v, contains := im.lights[k]

	if !contains {
		if round%2 == 0 {
			return 0
		} else {
			return 1
		}
	}

	if v {
		return 1
	}
	return 0
}

func (im *Image) enhance(round int) {
	m := make(map[aoc.Position]bool, len(im.lights))
	for row := im.minRow; row <= im.maxRow; row++ {
		for col := im.minCol; col <= im.maxCol; col++ {
			pos := aoc.Position{row, col}
			shift := im.get(round, pos, -1, -1)<<8 +
				im.get(round, pos, -1, 0)<<7 +
				im.get(round, pos, -1, 1)<<6 +
				im.get(round, pos, 0, -1)<<5 +
				im.get(round, pos, 0, 0)<<4 +
				im.get(round, pos, 0, 1)<<3 +
				im.get(round, pos, 1, -1)<<2 +
				im.get(round, pos, 1, 0)<<1 +
				im.get(round, pos, 1, 1)
			if im.enhancement[shift] == '#' {
				m[pos] = true
			} else {
				m[pos] = false
			}
		}
	}
	im.lights = m
}

func (im *Image) count() int {
	lit := 0
	for _, v := range im.lights {
		if v {
			lit++
		}
	}
	return lit
}

func toImage(groups [][]string) *Image {
	enhancement := groups[0][0]

	lights := make(map[aoc.Position]bool)
	const delta = 110
	for row := 0; row < len(groups[1]); row++ {
		for col := 0; col < len(groups[1][0]); col++ {
			if groups[1][row][col] == '#' {
				lights[aoc.Position{row, col}] = true
			} else {
				lights[aoc.Position{row, col}] = false
			}
		}
		for col := -delta; col < 0; col++ {
			lights[aoc.Position{row, col}] = false
		}
		for col := len(groups[1][0]); col < len(groups[1][0])+delta; col++ {
			lights[aoc.Position{row, col}] = false
		}
	}

	for row := -delta; row < 0; row++ {
		for col := -delta; col < 0; col++ {
			lights[aoc.Position{row, col}] = false
		}
		for col := len(groups[1][0]); col < len(groups[1][0])+delta; col++ {
			lights[aoc.Position{row, col}] = false
		}
	}

	for row := len(groups[1]); row < len(groups[1])+delta; row++ {
		for col := -delta; col < 0; col++ {
			lights[aoc.Position{row, col}] = false
		}
		for col := len(groups[1][0]); col < len(groups[1][0])+delta; col++ {
			lights[aoc.Position{row, col}] = false
		}
	}

	return &Image{
		enhancement: enhancement,
		lights:      lights,
		minRow:      -delta,
		minCol:      -delta,
		maxRow:      len(groups[1]) + delta,
		maxCol:      len(groups[1][0]) + delta,
	}
}

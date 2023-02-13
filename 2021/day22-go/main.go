package main

import (
	"bufio"
	"io"
	"strings"

	aoc "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	var instructions []Instruction
	for scanner.Scan() {
		line := scanner.Text()
		instructions = append(instructions, toInstruction(line))
	}

	sum := 0
	for x := -50; x <= 50; x++ {
		for y := -50; y <= 50; y++ {
			for z := -50; z <= 50; z++ {
				for i := len(instructions) - 1; i >= 0; i-- {
					ins := instructions[i]
					if !ins.isInside(x, y, z) {
						continue
					}
					if ins.on {
						sum++
					}
					break
				}
			}
		}
	}

	return sum
}

type Cube struct {
	on    bool
	count int
	from  Position
	to    Position
}

func newCube(from, to Position, on bool) *Cube {
	c := &Cube{
		on:    on,
		count: (to.x + 1 - from.x) * (to.y + 1 - from.y) * (to.z + 1 - from.z),
		from:  from,
		to:    to,
	}
	return c
}

func (c *Cube) overlap(c2 *Cube) (*Cube, bool) {
	if c.from.x > c2.to.x || c.to.x < c2.from.x {
		return nil, false
	}
	if c.from.y > c2.to.y || c.to.y < c2.from.y {
		return nil, false
	}
	if c.from.z > c2.to.z || c.to.z < c2.from.z {
		return nil, false
	}

	on := false
	if c.on && c2.on {
		on = false
	} else if !c.on && !c2.on {
		on = true
	} else {
		on = c2.on
	}

	return newCube(Position{
		x: aoc.Max(c.from.x, c2.from.x),
		y: aoc.Max(c.from.y, c2.from.y),
		z: aoc.Max(c.from.z, c2.from.z),
	}, Position{
		x: aoc.Min(c.to.x, c2.to.x),
		y: aoc.Min(c.to.y, c2.to.y),
		z: aoc.Min(c.to.z, c2.to.z),
	}, on), true
}

func toInstruction(s string) Instruction {
	s = strings.TrimSpace(s)

	spaces := aoc.NewDelimiter(s, " ")
	ins := Instruction{}
	if spaces.GetString(0) == "on" {
		ins.on = true
	}

	del := aoc.NewDelimiter(spaces.GetString(1), ",")
	delx := aoc.NewDelimiter(del.GetString(0)[2:], "..")
	dely := aoc.NewDelimiter(del.GetString(1)[2:], "..")
	delz := aoc.NewDelimiter(del.GetString(2)[2:], "..")
	ins.from.x = delx.GetInt(0)
	ins.to.x = delx.GetInt(1)
	ins.from.y = dely.GetInt(0)
	ins.to.y = dely.GetInt(1)
	ins.from.z = delz.GetInt(0)
	ins.to.z = delz.GetInt(1)
	return ins
}

type Instruction struct {
	on   bool
	from Position
	to   Position
}

func (ins Instruction) isInside(x, y, z int) bool {
	return x >= ins.from.x && x <= ins.to.x &&
		y >= ins.from.y && y <= ins.to.y &&
		z >= ins.from.z && z <= ins.to.z
}

type Position struct {
	x int
	y int
	z int
}

func fs2(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	var instructions []Instruction
	for scanner.Scan() {
		line := scanner.Text()
		instructions = append(instructions, toInstruction(line))
	}

	var cubes []*Cube
	cubes = append(cubes, newCube(instructions[0].from, instructions[0].to, true))
	for i := 1; i < len(instructions); i++ {
		ins := instructions[i]
		if ins.on {
			cubes = append(cubes, newCube(ins.from, ins.to, true))
		} else {
			cubes = append(cubes, newCube(ins.from, ins.to, false))
		}
	}

	var uniques []*Cube
	for _, c := range cubes {
		var tmp []*Cube
		for _, c2 := range uniques {
			if intersection, overlap := c2.overlap(c); overlap {
				tmp = append(tmp, intersection)
			}
		}
		if c.on {
			tmp = append(tmp, c)
		}
		uniques = append(uniques, tmp...)
	}

	var total int
	for _, c := range uniques {
		if c.on {
			total += c.count
		} else {
			total -= c.count
		}
	}

	return total
}

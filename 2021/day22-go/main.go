package main

import (
	"bufio"
	"io"

	aoc "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	var instructions []Instruction
	for scanner.Scan() {
		line := scanner.Text()
		instructions = append(instructions, toInstruction(line))
	}

	var cubes []*Cube
	cubes = append(cubes, newCube(instructions[0].from, instructions[0].to))
	for i := 1; i < len(instructions); i++ {
		ins := instructions[i]
		for _, cube1 := range cubes {
			cube1 := cube1
			if ins.on {
				cube2 := newCube(ins.from, ins.to)
				cube1.on(cube2)
				cubes = append(cubes, cube2)
			} else {
				cube1.off(cube1)
			}
		}
	}

	sum := 0
	for _, cube := range cubes {
		sum += cube.count
	}

	return sum
}

type Cube struct {
	count      int
	from       Position
	to         Position
	emptys     []*Cube
	duplicates []*Cube
}

func newCube(from, to Position) *Cube {
	return &Cube{
		count:  (to.x + 1 - from.x) * (to.y + 1 - from.y) * (to.z + 1 - from.z),
		from:   from,
		to:     to,
		emptys: nil,
	}
}

func (c *Cube) on(c2 *Cube) {
	v, over := c.overlap(c2)
	if !over {
		return
	}

	c.count -= v.count

	c.duplicates = append(c.duplicates, v)
}

func (c *Cube) off(c2 *Cube) {
	_, over := c.overlap(c2)
	if !over {
		return
	}

	return
}

//func merge(fromA, toA, fromB, toB int) int {
//	if fromA == toA && toA == toB {
//		return toA - fromA + 1
//	}
//	if fromB >= fromA && toB >= toA {
//		return toA - fromB + 1
//	}
//	if fromA <= fromB && toA >= toB {
//		return toB - fromB + 1
//	}
//	if fromB <= fromA && toB >= fromA && toB <= toA {
//		return toB - fromA + 1
//	}
//	if fromB <= fromA && toB >= toA {
//		return toA - fromA + 1
//	}
//	panic("not handled")
//}

func (c *Cube) inside(pos Position) bool {
	return pos.x >= c.from.x && pos.x <= c.to.x &&
		pos.y >= c.from.y && pos.y <= c.to.y &&
		pos.z >= c.from.z && pos.z <= c.to.z
}

func (c *Cube) area() int {
	return 0
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
	return newCube(Position{
		x: aoc.Max(c.from.x, c2.from.x),
		y: aoc.Max(c.from.y, c2.from.y),
		z: aoc.Max(c.from.z, c2.from.z),
	}, Position{
		x: aoc.Min(c.to.x, c2.to.x),
		y: aoc.Min(c.to.y, c2.to.y),
		z: aoc.Min(c.to.z, c2.to.z),
	}), true
}

func toInstruction(s string) Instruction {
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

type Position struct {
	x int
	y int
	z int
}

func fs2(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
		line := scanner.Text()
		_ = line
	}

	return 42
}

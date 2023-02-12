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
	count      int
	from       Position
	to         Position
	emptys     []*Cube
	duplicates []*Cube
}

func newCube(from, to Position) *Cube {
	c := &Cube{
		count:  (to.x + 1 - from.x) * (to.y + 1 - from.y) * (to.z + 1 - from.z),
		from:   from,
		to:     to,
		emptys: nil,
	}
	return c
}

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
	cubes = append(cubes, newCube(instructions[0].from, instructions[0].to))
	for i := 1; i < len(instructions); i++ {
		ins := instructions[i]
		cube2 := newCube(ins.from, ins.to)
		for _, cube1 := range cubes {
			cube1 := cube1
			if ins.on {
				cube1.on(cube2)
			} else {
				cube1.off(cube2)
			}
		}
		if ins.on {
			cubes = append(cubes, cube2)
		}
	}

	sum := 0
	for _, cube := range cubes {
		sum += cube.count
	}
	return sum
}

/*
----------------
            ----------
------------XXXX

*/
func (c *Cube) on(c2 *Cube) {
	v, over := c.overlap(c2)
	if !over {
		return
	}

	c.count -= v.count

	for _, c3 := range c.duplicates {
		v2, over2 := c3.overlap(c2)
		if !over2 {
			continue
		}
		c.count += v2.count
	}
	c.duplicates = append(c.duplicates, v)
}

/*
---......---
*/
func (c *Cube) off(c2 *Cube) {
	v, over := c.overlap(c2)
	if !over {
		return
	}

	c.count -= v.count

	for _, c3 := range c.emptys {
		v2, over2 := c3.overlap(c2)
		if !over2 {
			continue
		}
		c.count += v2.count
	}
	for _, c3 := range c.duplicates {
		v2, over2 := c3.overlap(c2)
		if !over2 {
			continue
		}
		c.count += v2.count
	}
	c.emptys = append(c.emptys, v)
}

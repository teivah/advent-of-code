package main

import (
	"fmt"
	"io"
	"strconv"

	aoc "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader, cycles int) int {
	lines := aoc.ReaderToStrings(input)
	cubes := make(map[Position]bool)

	for row := 0; row < len(lines); row++ {
		for col := 0; col < len(lines[row]); col++ {
			if lines[row][col] == '#' {
				cubes[Position{col, row, 0}] = true
			} else {
				cubes[Position{col, row, 0}] = false
			}
		}
	}

	c := &Cubes{cubes: cubes}
	for cycle := 0; cycle < cycles; cycle++ {
		c.round()
	}

	return c.countActives()
}

type Cubes struct {
	cubes      map[Position]bool
	hyperCubes map[HyperPosition]bool
}

func (c *Cubes) String() string {
	s := ""

	minerX := aoc.NewMiner()
	maxerX := aoc.NewMaxer()
	minerY := aoc.NewMiner()
	maxerY := aoc.NewMaxer()
	minerZ := aoc.NewMiner()
	maxerZ := aoc.NewMaxer()
	for pos := range c.cubes {
		minerX.Add(pos.x)
		maxerX.Add(pos.x)
		minerY.Add(pos.y)
		maxerY.Add(pos.y)
		minerZ.Add(pos.z)
		maxerZ.Add(pos.z)
	}

	for z := minerZ.Get(); z <= maxerZ.Get(); z++ {
		s += fmt.Sprintf("z=%d\n", z)

		s += "  "
		for x := minerX.Get(); x <= maxerX.Get(); x++ {
			s += strconv.Itoa(aoc.Abs(x))
		}
		s += "\n"

		for y := minerY.Get(); y <= maxerY.Get(); y++ {
			s += strconv.Itoa(aoc.Abs(y)) + " "
			for x := minerX.Get(); x <= maxerX.Get(); x++ {
				if c.cubes[Position{x, y, z}] {
					s += "#"
				} else {
					s += "."
				}
			}
			s += "\n"
		}
		s += "\n"
	}
	return s
}

func (c *Cubes) countNeighbors(pos Position) int {
	return c.countCube(pos.Delta(-1, -1, 0)) + // Level 0
		c.countCube(pos.Delta(0, -1, 0)) +
		c.countCube(pos.Delta(1, -1, 0)) +
		c.countCube(pos.Delta(-1, 0, 0)) +
		c.countCube(pos.Delta(1, 0, 0)) +
		c.countCube(pos.Delta(-1, 1, 0)) +
		c.countCube(pos.Delta(0, 1, 0)) +
		c.countCube(pos.Delta(1, 1, 0)) +
		c.countCube(pos.Delta(-1, -1, -1)) + // Level -1
		c.countCube(pos.Delta(0, -1, -1)) +
		c.countCube(pos.Delta(1, -1, -1)) +
		c.countCube(pos.Delta(-1, 0, -1)) +
		c.countCube(pos.Delta(0, 0, -1)) +
		c.countCube(pos.Delta(1, 0, -1)) +
		c.countCube(pos.Delta(-1, 1, -1)) +
		c.countCube(pos.Delta(0, 1, -1)) +
		c.countCube(pos.Delta(1, 1, -1)) +
		c.countCube(pos.Delta(-1, -1, 1)) + // Level 1
		c.countCube(pos.Delta(0, -1, 1)) +
		c.countCube(pos.Delta(1, -1, 1)) +
		c.countCube(pos.Delta(-1, 0, 1)) +
		c.countCube(pos.Delta(0, 0, 1)) +
		c.countCube(pos.Delta(1, 0, 1)) +
		c.countCube(pos.Delta(-1, 1, 1)) +
		c.countCube(pos.Delta(0, 1, 1)) +
		c.countCube(pos.Delta(1, 1, 1))
}

func (c *Cubes) countCube(pos Position) int {
	if c.cubes[pos] {
		return 1
	}
	return 0
}

func (c *Cubes) countActives() int {
	sum := 0
	for _, active := range c.cubes {
		if active {
			sum++
		}
	}
	return sum
}

func (c *Cubes) extendIfNotExist(extended map[Position]bool, pos Position) {
	if _, exists := c.cubes[pos]; !exists {
		extended[pos] = false
	}
}

func (c *Cubes) round() {
	extended := make(map[Position]bool)
	for pos, active := range c.cubes {
		extended[pos] = active

		for z := -1; z <= 1; z++ {
			for y := -1; y <= 1; y++ {
				for x := -1; x <= 1; x++ {
					c.extendIfNotExist(extended, Position{pos.x + x, pos.y + y, pos.z + z})
				}
			}
		}
	}

	res := make(map[Position]bool)
	for pos, active := range extended {
		count := c.countNeighbors(pos)

		if active {
			if count >= 2 && count <= 3 {
				res[pos] = true
			} else {
				res[pos] = false
			}
		} else {
			if count == 3 {
				res[pos] = true
			} else {
				res[pos] = false
			}
		}
	}

	c.cubes = res
}

type Position struct {
	x int
	y int
	z int
}

type HyperPosition struct {
	x int
	y int
	z int
	t int
}

func (p Position) Delta(x, y, z int) Position {
	return Position{
		x: p.x + x,
		y: p.y + y,
		z: p.z + z,
	}
}

func (p HyperPosition) Delta(x, y, z, t int) HyperPosition {
	return HyperPosition{
		x: p.x + x,
		y: p.y + y,
		z: p.z + z,
		t: p.t + t,
	}
}

func fs2(input io.Reader, cycles int) int {
	lines := aoc.ReaderToStrings(input)
	hypercubes := make(map[HyperPosition]bool)

	for row := 0; row < len(lines); row++ {
		for col := 0; col < len(lines[row]); col++ {
			if lines[row][col] == '#' {
				hypercubes[HyperPosition{col, row, 0, 0}] = true
			} else {
				hypercubes[HyperPosition{col, row, 0, 0}] = false
			}
		}
	}

	c := &Cubes{hyperCubes: hypercubes}
	for cycle := 0; cycle < cycles; cycle++ {
		c.hyperRound()
	}

	return c.countHyperActives()
}

func (c *Cubes) hyperRound() {
	extended := make(map[HyperPosition]bool)
	for pos, active := range c.hyperCubes {
		extended[pos] = active

		for z := -1; z <= 1; z++ {
			for y := -1; y <= 1; y++ {
				for x := -1; x <= 1; x++ {
					for t := -1; t <= 1; t++ {
						c.extendIfNotHyperExist(extended, HyperPosition{pos.x + x, pos.y + y, pos.z + z, pos.t + t})
					}
				}
			}
		}
	}

	res := make(map[HyperPosition]bool)
	for pos, active := range extended {
		count := c.countHyperNeighbors(pos)

		if active {
			if count >= 2 && count <= 3 {
				res[pos] = true
			} else {
				res[pos] = false
			}
		} else {
			if count == 3 {
				res[pos] = true
			} else {
				res[pos] = false
			}
		}
	}

	c.hyperCubes = res
}

func (c *Cubes) extendIfNotHyperExist(extended map[HyperPosition]bool, pos HyperPosition) {
	if _, exists := c.hyperCubes[pos]; !exists {
		extended[pos] = false
	}
}

func (c *Cubes) countHyperActives() int {
	sum := 0
	for _, active := range c.hyperCubes {
		if active {
			sum++
		}
	}
	return sum
}

func (c *Cubes) countHyperNeighbors(pos HyperPosition) int {
	sum := 0

	for z := -1; z <= 1; z++ {
		for y := -1; y <= 1; y++ {
			for x := -1; x <= 1; x++ {
				for t := -1; t <= 1; t++ {
					if x == 0 && y == 0 && z == 0 && t == 0 {
						continue
					}
					sum += c.countHyperCube(pos.Delta(x, y, z, t))
				}
			}
		}
	}

	return sum
}

func (c *Cubes) countHyperCube(pos HyperPosition) int {
	if c.hyperCubes[pos] {
		return 1
	}
	return 0
}

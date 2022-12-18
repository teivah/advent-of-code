package main

import (
	"bufio"
	"io"
	"strconv"
	"strings"
)

type Block struct {
	x int
	y int
	z int
}

type Grid struct {
	grid map[int]map[int]map[int]struct{}
}

func NewGrid() *Grid {
	return &Grid{
		grid: make(map[int]map[int]map[int]struct{}),
	}
}

func (g *Grid) add(x, y, z int) {
	v, exists := g.grid[x]
	if !exists {
		v = make(map[int]map[int]struct{})
		g.grid[x] = v
	}

	v2, exists := v[y]
	if !exists {
		v2 = make(map[int]struct{})
		v[y] = v2
	}

	v2[z] = struct{}{}
}

func (g *Grid) isEmpty(x, y, z int) int {
	v, exists := g.grid[x]
	if !exists {
		return 1
	}

	v2, exists := v[y]
	if !exists {
		return 1
	}

	_, exists = v2[z]
	if !exists {
		return 1
	}
	return 0
}

func fs1(input io.Reader) (int, error) {
	scanner := bufio.NewScanner(input)
	var blocks []Block
	grid := NewGrid()
	for scanner.Scan() {
		line := scanner.Text()
		split := strings.Split(line, ",")
		x, err := strconv.Atoi(split[0])
		if err != nil {
			return 0, err
		}
		y, err := strconv.Atoi(split[1])
		if err != nil {
			return 0, err
		}
		z, err := strconv.Atoi(split[2])
		if err != nil {
			return 0, err
		}

		grid.add(x, y, z)
		blocks = append(blocks, Block{x: x, y: y, z: z})
	}

	count := 0
	for _, block := range blocks {
		count += grid.isEmpty(block.x-1, block.y, block.z) +
			grid.isEmpty(block.x+1, block.y, block.z) +
			grid.isEmpty(block.x, block.y-1, block.z) +
			grid.isEmpty(block.x, block.y+1, block.z) +
			grid.isEmpty(block.x, block.y, block.z-1) +
			grid.isEmpty(block.x, block.y, block.z+1)

	}
	return count, nil
}

func fs2(input io.Reader) (int, error) {
	return 0, nil
}

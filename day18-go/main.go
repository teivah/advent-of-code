package main

import (
	"bufio"
	"io"
	"math"
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
	minX int
	minY int
	minZ int
	maxX int
	maxY int
	maxZ int
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

func (g *Grid) isEmpty(x, y, z int) bool {
	return g.countEmpty(x, y, z) == 1
}

func (g *Grid) countEmpty(x, y, z int) int {
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

func (g *Grid) countCube(x, y, z int) int {
	return 1 - g.countEmpty(x, y, z)
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
		count += grid.countEmpty(block.x-1, block.y, block.z) +
			grid.countEmpty(block.x+1, block.y, block.z) +
			grid.countEmpty(block.x, block.y-1, block.z) +
			grid.countEmpty(block.x, block.y+1, block.z) +
			grid.countEmpty(block.x, block.y, block.z-1) +
			grid.countEmpty(block.x, block.y, block.z+1)

	}
	return count, nil
}

func fs2(input io.Reader) (int, error) {
	scanner := bufio.NewScanner(input)
	var blocks []Block
	grid := NewGrid()

	minX := math.MaxInt
	minY := math.MaxInt
	minZ := math.MaxInt
	maxX := math.MinInt
	maxY := math.MinInt
	maxZ := math.MinInt
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

		minX = min(minX, x)
		minY = min(minY, y)
		minZ = min(minZ, z)
		maxX = max(maxX, x)
		maxY = max(maxY, y)
		maxZ = max(maxZ, z)

		grid.add(x, y, z)
		blocks = append(blocks, Block{x: x, y: y, z: z})
	}

	minX--
	minY--
	minZ--
	maxX++
	maxY++
	maxZ++

	grid.minX = minX
	grid.minY = minY
	grid.minZ = minZ
	grid.maxX = maxX
	grid.maxY = maxY
	grid.maxZ = maxZ

	visited = NewGrid()

	return dfs(grid, minX, minY, minZ), nil
}

var visited *Grid

func dfs(g *Grid, x, y, z int) int {
	if x < g.minX || y < g.minY || z < g.minZ || x > g.maxX || y > g.maxY || z > g.maxZ {
		return 0
	}

	if !g.isEmpty(x, y, z) {
		return 0
	}

	if !visited.isEmpty(x, y, z) {
		return 0
	}

	visited.add(x, y, z)

	count := g.countCube(x-1, y, z) +
		g.countCube(x+1, y, z) +
		g.countCube(x, y-1, z) +
		g.countCube(x, y+1, z) +
		g.countCube(x, y, z-1) +
		g.countCube(x, y, z+1)

	return count +
		dfs(g, x-1, y, z) +
		dfs(g, x+1, y, z) +
		dfs(g, x, y-1, z) +
		dfs(g, x, y+1, z) +
		dfs(g, x, y, z-1) +
		dfs(g, x, y, z+1)
}

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

package main

import (
	"bufio"
	"fmt"
	"io"
	"strconv"
	"strings"
)

func fs1(input io.Reader) (int, error) {
	scanner := bufio.NewScanner(input)
	scanner.Scan()
	scanner.Scan()
	var nodes []Node
	maxX := 0
	maxY := 0
	for scanner.Scan() {
		s := scanner.Text()
		dashes := indexAll(s, "-")
		spaces := indexAll(s, " ")
		x := toint(s[dashes[0]+2 : dashes[1]])
		y := toint(s[dashes[1]+2 : spaces[0]])

		ts := indexAll(s, "T")

		i := ts[0] - 1
		j := i - 1
		for ; ; j-- {
			if s[j] >= '0' && s[j] <= '9' {
				continue
			}
			break
		}
		size := toint(s[j+1 : i+1])

		i = ts[1] - 1
		j = i - 1
		for ; ; j-- {
			if s[j] >= '0' && s[j] <= '9' {
				continue
			}
			break
		}
		used := toint(s[j+1 : i+1])

		i = ts[2] - 1
		j = i - 1
		for ; ; j-- {
			if s[j] >= '0' && s[j] <= '9' {
				continue
			}
			break
		}
		available := toint(s[j+1 : i+1])

		i = len(s) - 2
		j = i - 1
		for ; ; j-- {
			if s[j] >= '0' && s[j] <= '9' {
				continue
			}
			break
		}
		use := toint(s[j+1 : i+1])

		nodes = append(nodes, Node{
			x:         x,
			y:         y,
			size:      size,
			used:      used,
			available: available,
			use:       use,
		})

		maxX = max(maxX, x)
		maxY = max(maxY, y)
	}

	sum := 0
	for i := 0; i < len(nodes); i++ {
		a := nodes[i]

		if a.used == 0 {
			continue
		}

		for j := 0; j < len(nodes); j++ {
			b := nodes[j]
			if isViable(a, b) {
				sum++
			}
		}
	}

	return sum, nil
}

func isViable(a, b Node) bool {
	return a.used != 0 && a != b && a.used <= b.available
}

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

type Node struct {
	x         int
	y         int
	size      int
	used      int
	available int
	use       int
}

func fs2(input io.Reader) (int, error) {
	scanner := bufio.NewScanner(input)
	scanner.Scan()
	var nodes []Node
	maxX := 0
	maxY := 0
	for scanner.Scan() {
		s := scanner.Text()
		dashes := indexAll(s, "-")
		spaces := indexAll(s, " ")
		x := toint(s[dashes[0]+2 : dashes[1]])
		y := toint(s[dashes[1]+2 : spaces[0]])

		ts := indexAll(s, "T")

		i := ts[0] - 1
		j := i - 1
		for ; ; j-- {
			if s[j] >= '0' && s[j] <= '9' {
				continue
			}
			break
		}
		size := toint(s[j+1 : i+1])

		i = ts[1] - 1
		j = i - 1
		for ; ; j-- {
			if s[j] >= '0' && s[j] <= '9' {
				continue
			}
			break
		}
		used := toint(s[j+1 : i+1])

		i = ts[2] - 1
		j = i - 1
		for ; ; j-- {
			if s[j] >= '0' && s[j] <= '9' {
				continue
			}
			break
		}
		available := toint(s[j+1 : i+1])

		i = len(s) - 2
		j = i - 1
		for ; ; j-- {
			if s[j] >= '0' && s[j] <= '9' {
				continue
			}
			break
		}
		use := toint(s[j+1 : i+1])

		nodes = append(nodes, Node{
			x:         x,
			y:         y,
			size:      size,
			used:      used,
			available: available,
			use:       use,
		})

		maxX = max(maxX, x)
		maxY = max(maxY, x)
	}

	grid := make([][]Node, maxY+1)
	for y := 0; y <= maxY; y++ {
		grid[y] = make([]Node, maxX+1)
	}

	for _, node := range nodes {
		grid[node.y][node.x] = node
	}

	fromX := maxX
	fromY := 0

	return bfs(grid, fromX, fromY, 0, 0), nil
}

type Grid struct {
	grid  [][]Node
	posX  int
	posY  int
	steps int
}

func newGrid(nodes [][]Node, x int, y int, steps int) Grid {
	res := make([][]Node, len(nodes))
	for row := 0; row < len(nodes); row++ {
		cols := len(nodes[0])
		res[row] = make([]Node, cols)

		for col := 0; col < cols; col++ {
			res[row][col] = nodes[row][col]
		}
	}

	return Grid{grid: nodes, posX: x, posY: y, steps: steps}
}

func move(grid Grid, fromX, fromY, deltaX, deltaY int) Grid {
	var res Grid
	toX := fromX + deltaX
	toY := fromY + deltaY

	if fromX == grid.posX && fromY == grid.posY {
		res = newGrid(grid.grid, toX, toY, grid.steps+1)
	} else {
		res = newGrid(grid.grid, grid.posX, grid.posY, grid.steps+1)
	}

	res.grid[toY][toX].used += res.grid[fromY][fromX].used
	res.grid[toY][toX].available -= res.grid[fromY][fromX].used
	res.grid[fromY][fromX].available += res.grid[fromY][fromX].used
	res.grid[fromY][fromX].used = 0
	return res
}

func printGrid(g Grid) {
	fmt.Println(g.steps)
	for row := 0; row < len(g.grid); row++ {
		for col := 0; col < len(g.grid[0]); col++ {
			fmt.Printf("%v/%v\t\t", g.grid[row][col].used, g.grid[row][col].available)
		}
		fmt.Println()
	}
	fmt.Println()
}

func key(g Grid) string {
	sb := strings.Builder{}
	for _, rows := range g.grid {
		for _, n := range rows {
			sb.WriteString(strconv.Itoa(n.used) + ".")
		}
	}
	return sb.String()
}

func bfs(start [][]Node, fromX, fromY, targetX, targetY int) int {
	var q []Grid
	q = append(q, newGrid(start, fromX, fromY, 0))

	for len(q) != 0 {
		g := q[0]
		q = q[1:]

		printGrid(g)

		if g.posX == targetX && g.posY == targetY {
			return g.steps
		}

		for row := 0; row < len(start); row++ {
			for col := 0; col < len(start[0]); col++ {
				current := g.grid[row][col]

				// Top
				if row > 0 {
					if enoughSpace(current, node(g, current, 0, -1)) {
						q = append(q, move(g, current.x, current.y, 0, -1))
					}
				}
				// Bottom
				if row < len(start)-1 {
					if enoughSpace(current, node(g, current, 0, 1)) {
						q = append(q, move(g, current.x, current.y, 0, 1))
					}
				}
				// Left
				if col > 0 {
					if enoughSpace(current, node(g, current, -1, 0)) {
						q = append(q, move(g, current.x, current.y, -1, 0))
					}
				}
				// Right
				if col < len(start[0])-1 {
					if enoughSpace(current, node(g, current, 1, 0)) {
						q = append(q, move(g, current.x, current.y, 1, 0))
					}
				}
			}
		}
	}

	return -1
}

func node(g Grid, node Node, deltaX, deltaY int) Node {
	return g.grid[node.y+deltaY][node.x+deltaX]
}

func enoughSpace(from, to Node) bool {
	return from.used <= to.available
}

func indexAll(s string, search string) []int {
	i := 0
	var res []int
	for i < len(s) {
		index := strings.Index(s[i:], search)
		if index == -1 {
			return res
		}
		res = append(res, index+i)
		i += index + len(search)
	}
	return res
}

func toint(s string) int {
	i, err := strconv.Atoi(s)
	if err != nil {
		panic(s)
	}
	return i
}

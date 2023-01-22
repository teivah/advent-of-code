package main

import (
	"bufio"
	"fmt"
	"io"
	"math"
	"strings"

	lib "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader, rounds int) int {
	scanner := bufio.NewScanner(input)
	var coords []Coord
	for scanner.Scan() {
		coords = append(coords, toCoord(scanner.Text()))
	}

	return draw(coords)
}

func draw(coords []Coord) int {
	minerCol := lib.NewMiner()
	minerRow := lib.NewMiner()
	maxerRow := lib.NewMaxer()
	maxerCol := lib.NewMaxer()

	for _, coord := range coords {
		minerCol.Add(coord.col)
		maxerCol.Add(coord.col)
		minerRow.Add(coord.row)
		maxerRow.Add(coord.row)
	}

	minCol := minerCol.Get()
	minRow := minerRow.Get()
	maxCol := maxerCol.Get()
	maxRow := maxerRow.Get()

	const buffer = 100

	grid := make(map[Coord]*Entry)
	for row := minRow - buffer; row < maxRow+buffer+1; row++ {
		for col := minCol - buffer; col < maxCol+buffer+1; col++ {
			grid[Coord{col, row}] = &Entry{dst: math.MaxInt, id: -1}
		}
	}

	for id, coord := range coords {
		for row := minRow - buffer; row < maxRow+buffer+1; row++ {
			for col := minCol - buffer; col < maxCol+buffer+1; col++ {
				dst := lib.Abs(row-coord.row) + lib.Abs(col-coord.col)
				key := Coord{col, row}
				if dst < grid[key].dst {
					grid[key].dst = dst
					grid[key].id = id
				} else if dst == grid[key].dst {
					grid[key].id = -1
				}
			}
		}
	}

	count := make(map[int]int)
	for row := minRow - buffer; row < maxRow+buffer+1; row++ {
		for col := minCol - buffer; col < maxCol+buffer+1; col++ {
			count[grid[Coord{col, row}].id]++
		}
	}

	dontConsider := make(map[int]struct{})
	for id, coord := range coords {
		visited := make(map[Coord]struct{})
		if keepGrowing(grid, id, coord, visited) {
			dontConsider[id] = struct{}{}
		}
	}

	maxer := lib.NewMaxer()
	for id, v := range count {
		if id == -1 {
			continue
		}
		if _, exists := dontConsider[id]; exists {
			continue
		}
		maxer.Add(v)
	}

	return maxer.Get()
}

func keepGrowing(grid map[Coord]*Entry, id int, coord Coord, visited map[Coord]struct{}) bool {
	_, exists := visited[coord]
	if exists {
		return false
	}
	visited[coord] = struct{}{}

	v, exists := grid[coord]
	if !exists {
		return true
	}

	if v.id != id {
		return false
	}

	return keepGrowing(grid, id, coord.delta(-1, 0), visited) ||
		keepGrowing(grid, id, coord.delta(1, 0), visited) ||
		keepGrowing(grid, id, coord.delta(0, -1), visited) ||
		keepGrowing(grid, id, coord.delta(0, 1), visited)
}

type Entry struct {
	dst int
	id  int
}

func bfs(coords []Coord, rounds int) int {
	var q []State
	for i, coord := range coords {
		q = append(q, State{i, coord})
	}

	grid := make(map[Coord]int)

	counts := make(map[int]int)
	previous := make(map[int]int)
	for i := 0; i < rounds; i++ {
		size := len(q)

		fmt.Println(i, len(q))
		roundGrid := make(map[Coord]int)

		for j := 0; j < size; j++ {
			if len(q) == 0 {
				break
			}

			s := q[0]
			q = q[1:]

			if _, exists := grid[s.coord]; exists {
				continue
			}

			v, exists := roundGrid[s.coord]
			if exists {
				if v != s.id {
					roundGrid[s.coord] = -1
				} else if v == s.id {
					continue
				}
			} else {
				roundGrid[s.coord] = s.id
			}

			q = append(q, State{s.id, s.coord.delta(-1, 0)})
			q = append(q, State{s.id, s.coord.delta(1, 0)})
			q = append(q, State{s.id, s.coord.delta(0, -1)})
			q = append(q, State{s.id, s.coord.delta(0, 1)})
		}

		for k, v := range roundGrid {
			_, exists := grid[k]
			if exists {
				continue
			}

			grid[k] = v
		}

		if i >= rounds-2 {
			previous = counts
			counts = make(map[int]int)
			for _, v := range grid {
				counts[v]++
			}
		}
	}

	notExpanding := make(map[int]struct{})
	for k, v := range counts {
		v2 := previous[k]
		if v == v2 {
			notExpanding[k] = struct{}{}
		}
	}

	sum := make(map[int]int)
	for _, v := range grid {
		if _, exists := notExpanding[v]; exists {
			sum[v]++
		}
	}

	max := 0
	for _, v := range sum {
		max = lib.Max(max, v)
	}
	return max
}

func printGrid(grid map[Coord]*Entry) {
	minerRow := lib.NewMiner()
	minerCol := lib.NewMiner()
	maxerRow := lib.NewMaxer()
	maxerCol := lib.NewMaxer()
	for coord := range grid {
		minerRow.Add(coord.row)
		maxerRow.Add(coord.row)
		minerCol.Add(coord.col)
		maxerCol.Add(coord.col)
	}

	for row := minerRow.Get(); row <= maxerRow.Get(); row++ {
		for col := minerCol.Get(); col <= maxerCol.Get(); col++ {
			v, exists := grid[Coord{col, row}]
			if !exists {
				fmt.Printf(" ")
			} else {
				if v.id == -1 {
					fmt.Printf(".")
				} else {
					fmt.Printf("%d", v.id)
				}
			}
		}
		fmt.Println()
	}
	fmt.Println()
}

type State struct {
	id    int
	coord Coord
}

type Coord struct {
	col int
	row int
}

func (c Coord) delta(row, col int) Coord {
	return Coord{
		col: c.col + col,
		row: c.row + row,
	}
}

func toCoord(s string) Coord {
	sep := strings.Index(s, ",")
	return Coord{
		col: lib.StringToInt(s[:sep]),
		row: lib.StringToInt(s[sep+2:]),
	}
}

func fs2(input io.Reader, less int) int {
	scanner := bufio.NewScanner(input)
	var coords []Coord
	for scanner.Scan() {
		coords = append(coords, toCoord(scanner.Text()))
	}

	return region(coords, less)
}

func region(coords []Coord, less int) int {
	minerCol := lib.NewMiner()
	minerRow := lib.NewMiner()
	maxerRow := lib.NewMaxer()
	maxerCol := lib.NewMaxer()

	for _, coord := range coords {
		minerCol.Add(coord.col)
		maxerCol.Add(coord.col)
		minerRow.Add(coord.row)
		maxerRow.Add(coord.row)
	}

	minCol := minerCol.Get()
	minRow := minerRow.Get()
	maxCol := maxerCol.Get()
	maxRow := maxerRow.Get()

	const buffer = 100

	grid := make(map[Coord]int)
	for row := minRow - buffer; row < maxRow+buffer+1; row++ {
		for col := minCol - buffer; col < maxCol+buffer+1; col++ {
			grid[Coord{col, row}] = 0
		}
	}

	for _, coord := range coords {
		for row := minRow - buffer; row < maxRow+buffer+1; row++ {
			for col := minCol - buffer; col < maxCol+buffer+1; col++ {
				dst := lib.Abs(row-coord.row) + lib.Abs(col-coord.col)
				key := Coord{col, row}
				grid[key] += dst
			}
		}
	}

	count := 0
	for row := minRow - buffer; row < maxRow+buffer+1; row++ {
		for col := minCol - buffer; col < maxCol+buffer+1; col++ {
			if grid[Coord{col, row}] < less {
				count++
			}
		}
	}

	return count
}

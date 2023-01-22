package main

import (
	"bufio"
	"fmt"
	"io"
	"strings"

	lib "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader, rounds int) int {
	scanner := bufio.NewScanner(input)
	var coords []Coord
	for scanner.Scan() {
		coords = append(coords, toCoord(scanner.Text()))
	}

	return bfs(coords, rounds)
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

		for j := 0; j < size; j++ {
			if len(q) == 0 {
				break
			}

			s := q[0]
			q = q[1:]

			_, exists := grid[s.coord]
			if !exists {
				grid[s.coord] = s.id
				q = append(q, s)
			}

			q = append(q, State{s.id, s.coord.delta(-1, 0)})
			q = append(q, State{s.id, s.coord.delta(1, 0)})
			q = append(q, State{s.id, s.coord.delta(0, -1)})
			q = append(q, State{s.id, s.coord.delta(0, 1)})
		}

		previous = counts
		counts = make(map[int]int)
		for _, v := range grid {
			counts[v]++
		}
	}

	notExpanding := make(map[int]struct{})
	for k, v := range counts {
		v2 := previous[k]
		if v == v2 {
			notExpanding[k] = struct{}{}
		}
	}

	//printGrid(grid)

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

func printGrid(grid map[Coord]int) {
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
				if v == -1 {
					fmt.Printf(".")
				} else {
					fmt.Printf("%d", v)
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

func fs2(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
		line := scanner.Text()
		_ = line
	}

	return 42
}

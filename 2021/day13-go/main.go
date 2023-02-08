package main

import (
	"fmt"
	"io"

	aoc "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	paper := toPaper(input)
	return paper.folds(false)
}

func (p *Paper) folds(all bool) int {
	for _, f := range p.fold {
		if f.Col == 0 {
			// Horizontal
			row := f.Row
			for col := 0; col < len(p.grid[0]); col++ {
				p.grid[row][col] = false
			}

			for col := 0; col < len(p.grid[0]); col++ {
				for deltaRow := 1; ; deltaRow++ {
					upRow := row - deltaRow
					if upRow < 0 {
						break
					}
					downRow := row + deltaRow
					p.grid[upRow][col] = p.grid[upRow][col] || p.grid[downRow][col]
				}
			}
			p.grid = p.grid[:row]
		} else {
			// Vertical
			col := f.Col

			for row := 0; row < len(p.grid); row++ {
				p.grid[row][col] = false
			}

			for row := 0; row < len(p.grid); row++ {
				for deltaCol := 1; ; deltaCol++ {
					leftCol := col - deltaCol
					if leftCol < 0 {
						break
					}
					rightCol := col + deltaCol
					p.grid[row][leftCol] = p.grid[row][leftCol] || p.grid[row][rightCol]
				}
			}

			for row := 0; row < len(p.grid); row++ {
				p.grid[row] = p.grid[row][:col]
			}
		}
		if !all {
			break
		}
	}

	if all {
		fmt.Println(p)
	}

	sum := 0
	for row := 0; row < len(p.grid); row++ {
		for col := 0; col < len(p.grid[0]); col++ {
			if p.grid[row][col] {
				sum++
			}
		}
	}
	return sum
}

func toPaper(input io.Reader) *Paper {
	lines := aoc.ReaderToStrings(input)
	groups := aoc.StringGroups(lines)

	var dots []aoc.Position
	maxRow := aoc.NewMaxer()
	maxCol := aoc.NewMaxer()
	for _, line := range groups[0] {
		del := aoc.NewDelimiter(line, ",")
		col := del.GetInt(0)
		row := del.GetInt(1)
		dots = append(dots, aoc.Position{
			Col: col,
			Row: row,
		})
		maxRow.Add(row)
		maxCol.Add(col)
	}

	var fold []aoc.Position
	for _, line := range groups[1] {
		del := aoc.NewDelimiter(line, " ")
		s := del.GetString(2)
		v := aoc.StringToInt(s[2:])

		if s[0] == 'y' {
			fold = append(fold, aoc.Position{
				Col: 0,
				Row: v,
			})
		} else {
			fold = append(fold, aoc.Position{
				Col: v,
				Row: 0,
			})
		}
	}

	grid := make([][]bool, maxRow.Get()+1)
	for row := 0; row <= maxRow.Get(); row++ {
		grid[row] = make([]bool, maxCol.Get()+1)
	}

	for _, dot := range dots {
		grid[dot.Row][dot.Col] = true
	}

	return &Paper{
		grid: grid,
		fold: fold,
	}
}

type Paper struct {
	grid [][]bool
	fold []aoc.Position
}

func (p *Paper) String() string {
	s := ""
	for row := 0; row < len(p.grid); row++ {
		for col := 0; col < len(p.grid[0]); col++ {
			if p.grid[row][col] {
				s += "#"
			} else {
				s += "."
			}
		}
		s += "\n"
	}
	return s
}

func fs2(input io.Reader) int {
	paper := toPaper(input)
	return paper.folds(true)
}

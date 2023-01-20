package main

import (
	"bufio"
	"fmt"
	"io"

	lib "github.com/teivah/advent-of-code"
)

type Unit struct {
	unitType UnitType
	letter   string
}

func toUnit(r rune) Unit {
	switch r {
	case '|':
		return Unit{unitType: Line}
	case '-':
		return Unit{unitType: Row}
	case '+':
		return Unit{unitType: Plus}
	case ' ':
		return Unit{unitType: Empty}
	default:
		return Unit{unitType: UnitTypeLetter, letter: string(r)}
	}
}

func (u Unit) String() string {
	switch u.unitType {
	case UnitTypeLetter:
		return u.letter
	case Line:
		return "|"
	case Row:
		return "-"
	case Plus:
		return "+"
	case Empty:
		return " "
	default:
		panic(u.unitType)
	}
}

type UnitType = int
type Heading = int

const (
	UnitTypeLetter UnitType = iota
	Line
	Row
	Plus
	Empty
)

const (
	Up Heading = iota
	Down
	Left
	Right
)

func fs1(input io.Reader) string {
	scanner := bufio.NewScanner(input)
	var grid [][]Unit
	maxCol := 0
	var pos Position
	for scanner.Scan() {
		line := scanner.Text()
		var row []Unit
		for i := 0; i < len(line); i++ {
			unit := toUnit(rune(line[i]))
			row = append(row, unit)
			if len(grid) == 0 && unit.unitType == Line {
				pos = Position{0, i, Down}
			}
		}
		maxCol = lib.Max(maxCol, len(line))
		grid = append(grid, row)
	}

	for row := range grid {
		for len(grid[row]) < maxCol {
			grid[row] = append(grid[row], Unit{unitType: Empty})
		}
	}

	res := ""
	for {
		if !pos.move(grid) {
			break
		}
		if grid[pos.row][pos.col].unitType == UnitTypeLetter {
			res += grid[pos.row][pos.col].letter
		}
	}

	return res
}

type Position struct {
	row     int
	col     int
	heading Heading
}

func (p *Position) move(grid [][]Unit) bool {
	if grid[p.row][p.col].unitType == Plus {
		if p.heading == Up || p.heading == Down {
			if grid[p.row][p.col-1].unitType != Empty {
				// Left
				p.col--
				p.heading = Left
			} else {
				// Right
				p.col++
				p.heading = Right
			}
		} else {
			if grid[p.row-1][p.col].unitType != Empty {
				// Up
				p.row--
				p.heading = Up
			} else {
				// Down
				p.row++
				p.heading = Down
			}
		}
		return true
	}

	switch p.heading {
	case Up:
		p.row--
	case Down:
		p.row++
	case Left:
		p.col--
	case Right:
		p.col++
	}

	return grid[p.row][p.col].unitType != Empty
}

func printGrid(grid [][]Unit) {
	for _, row := range grid {
		for _, col := range row {
			fmt.Printf("%v", col)
		}
		fmt.Println()
	}
}

func fs2(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	var grid [][]Unit
	maxCol := 0
	var pos Position
	for scanner.Scan() {
		line := scanner.Text()
		var row []Unit
		for i := 0; i < len(line); i++ {
			unit := toUnit(rune(line[i]))
			row = append(row, unit)
			if len(grid) == 0 && unit.unitType == Line {
				pos = Position{0, i, Down}
			}
		}
		maxCol = lib.Max(maxCol, len(line))
		grid = append(grid, row)
	}

	for row := range grid {
		for len(grid[row]) < maxCol {
			grid[row] = append(grid[row], Unit{unitType: Empty})
		}
	}

	steps := 0
	for {
		if !pos.move(grid) {
			break
		}
		steps++
	}

	return steps + 1
}

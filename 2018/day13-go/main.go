package main

import (
	"bufio"
	"fmt"
	"io"
	"sort"

	lib "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) string {
	scanner := bufio.NewScanner(input)
	var lines []string
	maxCol := 0
	for scanner.Scan() {
		line := scanner.Text()
		lines = append(lines, line)
		maxCol = lib.Max(maxCol, len(line))
	}
	maxRow := len(lines)

	var cars []*Car
	grid := make(map[Position]Unit)
	for row, line := range lines {
		for col := 0; col < len(line); col++ {
			switch line[col : col+1] {
			case "\\":
				grid[Position{row, col}] = Unit{unitType: CurveLeft}
			case "/":
				grid[Position{row, col}] = Unit{unitType: CurveRight}
			case "|":
				grid[Position{row, col}] = Unit{unitType: Vertical}
			case "-":
				grid[Position{row, col}] = Unit{unitType: Horizontal}
			case "+":
				grid[Position{row, col}] = Unit{unitType: Intersection}
			case ">":
				grid[Position{row, col}] = Unit{unitType: Unknown}
				cars = append(cars, &Car{id: len(cars), direction: Right, position: Position{row, col}})
			case "<":
				grid[Position{row, col}] = Unit{unitType: Unknown}
				cars = append(cars, &Car{id: len(cars), direction: Left, position: Position{row, col}})
			case "^":
				grid[Position{row, col}] = Unit{unitType: Unknown}
				cars = append(cars, &Car{id: len(cars), direction: Up, position: Position{row, col}})
			case "v":
				grid[Position{row, col}] = Unit{unitType: Unknown}
				cars = append(cars, &Car{id: len(cars), direction: Down, position: Position{row, col}})
			}
		}
	}

	b := Board{
		grid:   grid,
		cars:   cars,
		maxRow: maxRow,
		maxCol: maxCol,
	}

	for {
		collision, p := b.move()
		if collision {
			return fmt.Sprintf("%d,%d", p.col, p.row)
		}
	}
}

type Board struct {
	grid   map[Position]Unit
	cars   []*Car
	maxRow int
	maxCol int
}

func (b Board) move() (bool, Position) {
	sort.Slice(b.cars, func(i, j int) bool {
		x := b.cars[i]
		y := b.cars[j]
		if x.position.row < y.position.row {
			return true
		}
		if y.position.row < x.position.row {
			return false
		}
		return x.position.col <= y.position.col
	})

	for _, car := range b.cars {
		cur := b.grid[car.position]

		switch cur.unitType {
		case CurveLeft:
			switch car.direction {
			case Up:
				car.position = car.position.delta(0, -1)
				car.direction = Left
			case Down:
				car.position = car.position.delta(0, 1)
				car.direction = Right
			case Left:
				car.position = car.position.delta(-1, 0)
				car.direction = Up
			case Right:
				car.position = car.position.delta(1, 0)
				car.direction = Down
			}
		case CurveRight:
			switch car.direction {
			case Up:
				car.position = car.position.delta(0, 1)
				car.direction = Right
			case Down:
				car.position = car.position.delta(0, -1)
				car.direction = Left
			case Left:
				car.position = car.position.delta(1, 0)
				car.direction = Down
			case Right:
				car.position = car.position.delta(-1, 0)
				car.direction = Up
			}
		case Horizontal, Vertical, Unknown:
			switch car.direction {
			case Up:
				car.position = car.position.delta(-1, 0)
			case Down:
				car.position = car.position.delta(1, 0)
			case Left:
				car.position = car.position.delta(0, -1)
			case Right:
				car.position = car.position.delta(0, 1)
			}
		case Intersection:
			car.nextMove()
			switch car.direction {
			case Up:
				car.position = car.position.delta(-1, 0)
			case Down:
				car.position = car.position.delta(1, 0)
			case Left:
				car.position = car.position.delta(0, -1)
			case Right:
				car.position = car.position.delta(0, 1)
			}
		}

		if b.isCollision(car.id, car.position) {
			return true, car.position
		}
	}
	return false, Position{}
}

func (b *Board) move2() {
	sort.Slice(b.cars, func(i, j int) bool {
		x := b.cars[i]
		y := b.cars[j]
		if x.position.row < y.position.row {
			return true
		}
		if y.position.row < x.position.row {
			return false
		}
		return x.position.col <= y.position.col
	})

	keep := make(map[int]struct{}, len(b.cars))
	for _, car := range b.cars {
		keep[car.id] = struct{}{}
	}

	for _, car := range b.cars {
		cur := b.grid[car.position]

		switch cur.unitType {
		case CurveLeft:
			switch car.direction {
			case Up:
				car.position = car.position.delta(0, -1)
				car.direction = Left
			case Down:
				car.position = car.position.delta(0, 1)
				car.direction = Right
			case Left:
				car.position = car.position.delta(-1, 0)
				car.direction = Up
			case Right:
				car.position = car.position.delta(1, 0)
				car.direction = Down
			}
		case CurveRight:
			switch car.direction {
			case Up:
				car.position = car.position.delta(0, 1)
				car.direction = Right
			case Down:
				car.position = car.position.delta(0, -1)
				car.direction = Left
			case Left:
				car.position = car.position.delta(1, 0)
				car.direction = Down
			case Right:
				car.position = car.position.delta(-1, 0)
				car.direction = Up
			}
		case Horizontal, Vertical, Unknown:
			switch car.direction {
			case Up:
				car.position = car.position.delta(-1, 0)
			case Down:
				car.position = car.position.delta(1, 0)
			case Left:
				car.position = car.position.delta(0, -1)
			case Right:
				car.position = car.position.delta(0, 1)
			}
		case Intersection:
			car.nextMove()
			switch car.direction {
			case Up:
				car.position = car.position.delta(-1, 0)
			case Down:
				car.position = car.position.delta(1, 0)
			case Left:
				car.position = car.position.delta(0, -1)
			case Right:
				car.position = car.position.delta(0, 1)
			}
		}

		if b.isCollision(car.id, car.position) {
			b.removeIDs(keep, car.position)
		}
	}

	newCars := make([]*Car, 0, len(keep))
	for _, car := range b.cars {
		if _, exists := keep[car.id]; exists {
			newCars = append(newCars, car)
		}
	}

	b.cars = newCars
}

func (b Board) removeIDs(set map[int]struct{}, pos Position) {
	for _, car := range b.cars {
		if car.position == pos {
			delete(set, car.id)
		}
	}
}

func (b Board) isCollision(id int, pos Position) bool {
	for _, car := range b.cars {
		if id != car.id && car.position == pos {
			return true
		}
	}
	return false
}

func (b Board) isCar(pos Position) bool {
	for _, car := range b.cars {
		if car.position == pos {
			return true
		}
	}
	return false
}

func (b Board) print() {
	for row := 0; row < b.maxRow; row++ {
		for col := 0; col < b.maxCol; col++ {
			k := Position{row, col}

			if b.isCar(k) {
				fmt.Printf("X")
				continue
			}

			if v, exists := b.grid[k]; exists {
				switch v.unitType {
				case CurveLeft:
					fmt.Printf("\\")
				case CurveRight:
					fmt.Printf("/")
				case Horizontal:
					fmt.Printf("-")
				case Vertical:
					fmt.Printf("|")
				case Intersection:
					fmt.Printf("+")
				case Unknown:
					fmt.Printf(".")
				}
			} else {
				fmt.Printf(" ")
			}
		}
		fmt.Println()
	}
	fmt.Println()
}

type Car struct {
	id        int
	direction Direction
	position  Position
	turns     int
}

func (c *Car) nextMove() {
	defer func() { c.turns++ }()
	switch c.turns % 3 {
	case 0:
		switch c.direction {
		case Up:
			c.direction = Left
		case Down:
			c.direction = Right
		case Left:
			c.direction = Down
		case Right:
			c.direction = Up
		}
		return
	case 1:
		return
	case 2:
		switch c.direction {
		case Up:
			c.direction = Right
		case Down:
			c.direction = Left
		case Left:
			c.direction = Up
		case Right:
			c.direction = Down
		}
		return
	}
	panic(c.turns)
}

type Unit struct {
	unitType UnitType
}

type UnitType int

const (
	CurveRight UnitType = iota
	CurveLeft
	Horizontal
	Vertical
	Intersection
	Unknown
)

type Direction int

const (
	Up Direction = iota
	Down
	Left
	Right
)

type Position struct {
	row int
	col int
}

func (p Position) delta(row, col int) Position {
	p.row += row
	p.col += col
	return p
}

func fs2(input io.Reader) string {
	scanner := bufio.NewScanner(input)
	var lines []string
	maxCol := 0
	for scanner.Scan() {
		line := scanner.Text()
		lines = append(lines, line)
		maxCol = lib.Max(maxCol, len(line))
	}
	maxRow := len(lines)

	var cars []*Car
	grid := make(map[Position]Unit)
	for row, line := range lines {
		for col := 0; col < len(line); col++ {
			switch line[col : col+1] {
			case "\\":
				grid[Position{row, col}] = Unit{unitType: CurveLeft}
			case "/":
				grid[Position{row, col}] = Unit{unitType: CurveRight}
			case "|":
				grid[Position{row, col}] = Unit{unitType: Vertical}
			case "-":
				grid[Position{row, col}] = Unit{unitType: Horizontal}
			case "+":
				grid[Position{row, col}] = Unit{unitType: Intersection}
			case ">":
				grid[Position{row, col}] = Unit{unitType: Unknown}
				cars = append(cars, &Car{id: len(cars), direction: Right, position: Position{row, col}})
			case "<":
				grid[Position{row, col}] = Unit{unitType: Unknown}
				cars = append(cars, &Car{id: len(cars), direction: Left, position: Position{row, col}})
			case "^":
				grid[Position{row, col}] = Unit{unitType: Unknown}
				cars = append(cars, &Car{id: len(cars), direction: Up, position: Position{row, col}})
			case "v":
				grid[Position{row, col}] = Unit{unitType: Unknown}
				cars = append(cars, &Car{id: len(cars), direction: Down, position: Position{row, col}})
			}
		}
	}

	b := Board{
		grid:   grid,
		cars:   cars,
		maxRow: maxRow,
		maxCol: maxCol,
	}

	for {
		b.move2()
		if len(b.cars) == 1 {
			return fmt.Sprintf("%d,%d", b.cars[0].position.col, b.cars[0].position.row)
		}
	}
}

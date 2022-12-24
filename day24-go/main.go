package main

import (
	"bufio"
	"fmt"
	"io"
	"math"
)

func fs1(input io.Reader, multiplier int) (int, error) {
	scanner := bufio.NewScanner(input)
	var lines []string
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}

	board := toBoard(lines, multiplier)

	res := board.move(0, board.current.row, board.current.col, board.to.row, board.to.col)

	return res - 1, nil
}

func toBoard(lines []string, multiplier int) *Board {
	row := len(lines)
	col := len(lines[0])
	board := make([][]Unit, 0, row)

	first := make([]Unit, col)
	for i := 0; i < col; i++ {
		first[i] = Unit{unitType: WallUnit}
	}
	first[1].unitType = EmptyUnit
	board = append(board, first)

	for i := 1; i < len(lines)-1; i++ {
		cur := make([]Unit, col)
		line := lines[i]
		for j := 0; j < len(line); j++ {
			switch line[j] {
			case '#':
				cur[j] = Unit{unitType: WallUnit}
			case '.':
				cur[j] = Unit{unitType: EmptyUnit}
			case '^':
				cur[j] = Unit{unitType: BlizzardUnit, blizzards: []Direction{Up}}
			case 'v':
				cur[j] = Unit{unitType: BlizzardUnit, blizzards: []Direction{Down}}
			case '<':
				cur[j] = Unit{unitType: BlizzardUnit, blizzards: []Direction{Left}}
			case '>':
				cur[j] = Unit{unitType: BlizzardUnit, blizzards: []Direction{Right}}
			}
		}
		board = append(board, cur)
	}

	last := make([]Unit, col)
	for i := 0; i < col; i++ {
		last[i] = Unit{unitType: WallUnit}
	}
	last[col-2].unitType = EmptyUnit
	board = append(board, last)
	boards := make([][][]Unit, 1, multiplier)
	boards[0] = board

	b := &Board{
		row:        row,
		col:        col,
		current:    Position{row: 0, col: 1},
		from:       Position{row: 0, col: 1},
		to:         Position{row: row - 1, col: col - 2},
		multiplier: multiplier,
		visited:    make(map[int]map[Position]int),
	}

	previous := board
	for i := 1; i < multiplier; i++ {
		next := b.newBoard(previous)
		boards = append(boards, next)
		previous = next
	}

	b.boards = boards
	return b
}

func (b *Board) resetCache() {
	b.visited = make(map[int]map[Position]int)
}

func (b *Board) setVisited(round int, row int, col int) {
	m, exists := b.visited[round%b.multiplier]
	if !exists {
		m = make(map[Position]int)
		b.visited[round%b.multiplier] = m
	}

	m[Position{row, col}] = round
}

func (b *Board) isVisited(round int, row int, col int) bool {
	m, exists := b.visited[round%b.multiplier]
	if !exists {
		return false
	}

	v, exists := m[Position{row, col}]
	if !exists {
		return false
	}

	if round < v {
		return false
	}
	return true
}

func (b *Board) move(round int, row, col, toRow, toCol int) int {
	// TODO Precompute blizzards

	if row == toRow && col == toCol {
		return round
	}

	if b.isVisited(round, row, col) {
		return math.MaxInt
	}

	b.setVisited(round, row, col)

	best := math.MaxInt

	// Move right
	if b.isAllowed(round, row, col+1) {
		best = min(best, b.move(round+1, row, col+1, toRow, toCol))
	}
	// Move left
	if b.isAllowed(round, row, col-1) {
		best = min(best, b.move(round+1, row, col-1, toRow, toCol))
	}
	// Move down
	if b.isAllowed(round, row+1, col) {
		best = min(best, b.move(round+1, row+1, col, toRow, toCol))
	}
	// Move top
	if b.isAllowed(round, row-1, col) {
		best = min(best, b.move(round+1, row-1, col, toRow, toCol))
	}
	// Stay same position
	if b.isAllowed(round, row, col) {
		best = min(best, b.move(round+1, row, col, toRow, toCol))
	}

	return best
}

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func (b *Board) isAllowed(round int, row, col int) bool {
	if row < 0 {
		return false
	}

	if row >= b.row {
		return false
	}

	board := b.boards[round%b.multiplier]

	return board[row][col].unitType == EmptyUnit
}

func (b *Board) newBoard(previous [][]Unit) [][]Unit {
	board := make([][]Unit, b.row)
	for row := 0; row < b.row; row++ {
		board[row] = make([]Unit, b.col)
		for col := 0; col < b.col; col++ {
			switch previous[row][col].unitType {
			case WallUnit:
				board[row][col].unitType = WallUnit
			default:
				board[row][col].unitType = EmptyUnit
			}
		}
	}

	for row := 0; row < b.row; row++ {
		for col := 0; col < b.col; col++ {
			if previous[row][col].unitType != BlizzardUnit {
				continue
			}

			for _, blizzard := range previous[row][col].blizzards {
				switch blizzard {
				case Up:
					if previous[row-1][col].unitType == WallUnit {
						pos := b.getOpposite(Position{row: row - 1, col: col})
						board[pos.row][pos.col].unitType = BlizzardUnit
						board[pos.row][pos.col].blizzards = append(board[pos.row][pos.col].blizzards, blizzard)
					} else {
						board[row-1][col].unitType = BlizzardUnit
						board[row-1][col].blizzards = append(board[row-1][col].blizzards, blizzard)
					}
				case Down:
					if previous[row+1][col].unitType == WallUnit {
						pos := b.getOpposite(Position{row: row + 1, col: col})
						board[pos.row][pos.col].unitType = BlizzardUnit
						board[pos.row][pos.col].blizzards = append(board[pos.row][pos.col].blizzards, blizzard)
					} else {
						board[row+1][col].unitType = BlizzardUnit
						board[row+1][col].blizzards = append(board[row+1][col].blizzards, blizzard)
					}
				case Left:
					if previous[row][col-1].unitType == WallUnit {
						pos := b.getOpposite(Position{row: row, col: col - 1})
						board[pos.row][pos.col].unitType = BlizzardUnit
						board[pos.row][pos.col].blizzards = append(board[pos.row][pos.col].blizzards, blizzard)
					} else {
						board[row][col-1].unitType = BlizzardUnit
						board[row][col-1].blizzards = append(board[row][col-1].blizzards, blizzard)
					}
				case Right:
					if previous[row][col+1].unitType == WallUnit {
						pos := b.getOpposite(Position{row: row, col: col + 1})
						board[pos.row][pos.col].unitType = BlizzardUnit
						board[pos.row][pos.col].blizzards = append(board[pos.row][pos.col].blizzards, blizzard)
					} else {
						board[row][col+1].unitType = BlizzardUnit
						board[row][col+1].blizzards = append(board[row][col+1].blizzards, blizzard)
					}
				}
			}
		}
	}

	return board
}

func (b *Board) getOpposite(position Position) Position {
	if position.row == 0 {
		return Position{row: b.row - 2, col: position.col}
	}
	if position.row == b.row-1 {
		return Position{row: 1, col: position.col}
	}
	if position.col == 0 {
		return Position{row: position.row, col: b.col - 2}
	}
	if position.col == b.col-1 {
		return Position{row: position.row, col: 1}
	}
	panic("get opposite")
}

func print(board [][]Unit) {
	for _, row := range board {
		for _, unit := range row {
			switch unit.unitType {
			case EmptyUnit:
				fmt.Print(".")
			case WallUnit:
				fmt.Print("#")
			case BlizzardUnit:
				if len(unit.blizzards) > 1 {
					fmt.Printf("%d", len(unit.blizzards))
				} else {
					switch unit.blizzards[0] {
					case Up:
						fmt.Print("^")
					case Down:
						fmt.Print("v")
					case Left:
						fmt.Print("<")
					case Right:
						fmt.Print(">")
					}
				}
			}
		}
		fmt.Println()
	}
	fmt.Println()
}

type Board struct {
	row     int
	col     int
	from    Position
	to      Position
	current Position
	boards  [][][]Unit

	multiplier int
	visited    map[int]map[Position]int
}

type Position struct {
	row int
	col int
}

type Unit struct {
	unitType  UnitType
	blizzards []Direction
}

type Direction int

const (
	Up Direction = iota
	Down
	Left
	Right
)

type UnitType int

const (
	EmptyUnit UnitType = iota
	WallUnit
	BlizzardUnit
)

func fs2(input io.Reader, multiplier int) (int, error) {
	scanner := bufio.NewScanner(input)
	var lines []string
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}

	board := toBoard(lines, multiplier)

	fromRow := board.from.row
	fromCol := board.from.col
	toRow := board.to.row
	toCol := board.to.col

	firstTrip := board.move(0, fromRow, fromCol, toRow, toCol)

	board.resetCache()
	secondTrip := board.move(firstTrip, toRow, toCol, fromRow, fromCol)

	board.resetCache()
	thirdTrip := board.move(secondTrip, fromRow, fromCol, toRow, toCol)

	return thirdTrip - 1, nil
}

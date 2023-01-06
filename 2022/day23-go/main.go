package main

import (
	"bufio"
	"fmt"
	"io"
	"math"
)

func fs1(input io.Reader, count int) (int, error) {
	scanner := bufio.NewScanner(input)
	var board [][]*Elf
	elves := make(map[int]*Elf)
	row := 0
	id := 0
	for scanner.Scan() {
		line := scanner.Text()
		if len(line) == 0 {
			continue
		}
		b := make([]*Elf, len(line))
		for i := 0; i < len(line); i++ {
			if line[i] == '#' {
				elf := Elf{row: row, col: i, id: id}
				elves[id] = &elf
				b[i] = &elf
				id++
			}
		}
		board = append(board, b)
		row++
	}

	b := newBoard(board, elves)
	//b.print()

	for i := 0; i < count; i++ {
		b.round()
		//fmt.Printf("== End of Round == %v\n", i+1)
		//b.print()
	}

	minRow := math.MaxInt
	minCol := math.MaxInt
	maxRow := -1
	maxCol := -1
	for _, elve := range b.elves {
		minRow = min(minRow, elve.row)
		minCol = min(minCol, elve.col)
		maxRow = max(maxRow, elve.row)
		maxCol = max(maxCol, elve.col)
	}

	res := (maxRow-minRow+1)*(maxCol-minCol+1) - len(b.elves)

	return res, nil
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

type Elf struct {
	id  int
	row int
	col int
}

type Proposition struct {
	id  int
	row int
	col int
}

func (e *Elf) proposition(b *Board) *Proposition {
	if e.isEmpty(b, -1, -1) &&
		e.isEmpty(b, -1, 0) &&
		e.isEmpty(b, -1, 1) &&
		e.isEmpty(b, 0, -1) &&
		e.isEmpty(b, 0, 1) &&
		e.isEmpty(b, 1, -1) &&
		e.isEmpty(b, 1, 0) &&
		e.isEmpty(b, 1, 1) {
		return nil
	}

	for _, directions := range b.directions {
		possible := true
		for _, direction := range directions.directions {
			if !e.isEmpty(b, direction.shiftRow, direction.shiftCol) {
				possible = false
				break
			}
		}
		if possible {
			return &Proposition{
				id:  e.id,
				row: e.row + directions.move.shiftRow,
				col: e.col + directions.move.shiftCol,
			}
		}
	}
	return nil
}

func (e *Elf) isEmpty(b *Board, shiftRow, shiftCol int) bool {
	row := e.row + shiftRow
	col := e.col + shiftCol

	if row < 0 || row >= b.row || col < 0 || col >= b.col {
		return true
	}

	return b.board[row][col] == nil
}

func (e *Elf) move(b *Board, proposition Proposition, expandUp, expandLeft bool) {
	oldRow := e.row
	oldCol := e.col
	newRow := proposition.row
	newCol := proposition.col

	if expandUp && expandLeft {
		b.board[newRow+1][newCol+1] = e
		e.row = newRow + 1
		e.col = newCol + 1
	} else if expandLeft {
		b.board[newRow][newCol+1] = e
		e.row = newRow
		e.col = newCol + 1
	} else if expandUp {
		b.board[newRow+1][newCol] = e
		e.row = newRow + 1
		e.col = newCol
	} else {
		b.board[newRow][newCol] = e
		e.row = newRow
		e.col = newCol
	}
	b.board[oldRow][oldCol] = nil
}

type Board struct {
	board      [][]*Elf
	elves      map[int]*Elf
	directions []Directions
	row        int
	col        int
}

type Direction struct {
	shiftRow int
	shiftCol int
}

type Directions struct {
	directions []Direction
	move       Direction
}

func newBoard(board [][]*Elf, elves map[int]*Elf) *Board {
	var directions []Directions

	directions = append(directions, Directions{
		directions: []Direction{
			{shiftRow: -1, shiftCol: 0},
			{shiftRow: -1, shiftCol: 1},
			{shiftRow: -1, shiftCol: -1},
		},
		move: Direction{shiftRow: -1, shiftCol: 0},
	})
	directions = append(directions, Directions{
		directions: []Direction{
			{shiftRow: 1, shiftCol: 0},
			{shiftRow: 1, shiftCol: 1},
			{shiftRow: 1, shiftCol: -1},
		},
		move: Direction{shiftRow: 1, shiftCol: 0},
	})
	directions = append(directions, Directions{
		directions: []Direction{
			{shiftRow: 0, shiftCol: -1},
			{shiftRow: 1, shiftCol: -1},
			{shiftRow: -1, shiftCol: -1},
		},
		move: Direction{shiftRow: 0, shiftCol: -1},
	})
	directions = append(directions, Directions{
		directions: []Direction{
			{shiftRow: 0, shiftCol: 1},
			{shiftRow: 1, shiftCol: 1},
			{shiftRow: -1, shiftCol: 1},
		},
		move: Direction{shiftRow: 0, shiftCol: 1},
	})

	return &Board{
		board:      board,
		elves:      elves,
		directions: directions,
		row:        len(board),
		col:        len(board[0]),
	}
}

func (b *Board) round() bool {
	var propositions []Proposition
	for _, elf := range b.elves {
		if prop := elf.proposition(b); prop != nil {
			propositions = append(propositions, *prop)
		}
	}

	m := make(map[Proposition]int)
	for _, key := range propositions {
		value := key.id
		key.id = 0
		_, exists := m[key]
		if exists {
			delete(m, key)
		} else {
			m[key] = value
		}
	}

	if len(m) == 0 {
		return false
	}

	expandUp, expandLeft := b.arrangeBoard(m)

	for proposition, id := range m {
		b.elves[id].move(b, proposition, expandUp, expandLeft)
	}

	b.nextDirections()
	return true
}

func (b *Board) arrangeBoard(m map[Proposition]int) (bool, bool) {
	expandUp := false
	expandDown := false
	expandLeft := false
	expandRight := false

	for proposition := range m {
		if proposition.row < 0 {
			expandUp = true
		}
		if proposition.col < 0 {
			expandLeft = true
		}
		if proposition.row == b.row {
			expandDown = true
		}
		if proposition.col == b.col {
			expandRight = true
		}
	}

	if expandDown {
		b.board = append(b.board, make([]*Elf, b.col))
		b.row++
	}

	if expandRight {
		for i := range b.board {
			b.board[i] = append(b.board[i], nil)
		}
		b.col++
	}

	if expandUp {
		newBoard := make([][]*Elf, 1, len(b.board)+1)
		newBoard[0] = make([]*Elf, b.col)
		for _, board := range b.board {
			newBoard = append(newBoard, board)
		}
		b.board = newBoard
		b.row++
	}

	if expandLeft {
		for i := range b.board {
			newCol := make([]*Elf, 1, b.col+1)
			newCol = append(newCol, b.board[i]...)
			b.board[i] = newCol
		}
		b.col++
	}

	if expandLeft {
		for _, elf := range b.elves {
			elf.col++
		}
	}

	if expandUp {
		for _, elf := range b.elves {
			elf.row++
		}
	}

	return expandUp, expandLeft
}

func (b *Board) nextDirections() {
	b.directions = append(b.directions[1:], b.directions[0])
}

func (b *Board) print() {
	for _, row := range b.board {
		for _, col := range row {
			if col == nil {
				fmt.Print(".")
			} else {
				fmt.Print("#")
				//fmt.Printf("%v", col.id)
			}
		}
		fmt.Println()
	}
	fmt.Println()
}

func fs2(input io.Reader) (int, error) {
	scanner := bufio.NewScanner(input)
	var board [][]*Elf
	elves := make(map[int]*Elf)
	row := 0
	id := 0
	for scanner.Scan() {
		line := scanner.Text()
		if len(line) == 0 {
			continue
		}
		b := make([]*Elf, len(line))
		for i := 0; i < len(line); i++ {
			if line[i] == '#' {
				elf := Elf{row: row, col: i, id: id}
				elves[id] = &elf
				b[i] = &elf
				id++
			}
		}
		board = append(board, b)
		row++
	}

	b := newBoard(board, elves)
	//b.print()

	for i := 0; ; i++ {
		if !b.round() {
			return i + 1, nil
		}
		//fmt.Printf("== End of Round == %v\n", i+1)
		//b.print()
	}
}

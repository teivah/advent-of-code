package main

import (
	"bufio"
	"fmt"
	"io"
	"strconv"
)

func fs1(input io.Reader) (int, error) {
	scanner := bufio.NewScanner(input)
	var lines []string
	for scanner.Scan() {
		line := scanner.Text()
		lines = append(lines, line)
	}

	board := toBoard(lines[:len(lines)-2])

	steps := toSteps(lines[len(lines)-1])

	for _, step := range steps {
		if step.isMove {
			for i := 0; i < step.move; i++ {
				isBlocked := board.move()
				if isBlocked {
					break
				}
				//board.print()
			}
		} else {
			if step.turn == Left {
				board.left()
			} else {
				board.right()
			}
			//board.print()
		}
	}

	res := (board.curRow+1)*1000 + (board.curCol+1)*4 + board.getDirectionValue()
	return res, nil
}

type Board struct {
	board  [][]Position
	maxRow int
	maxCol int

	curRow       int
	curCol       int
	curDirection *Direction
}

type Direction struct {
	direction DirectionType
}

func (d *Direction) left() {
	switch d.direction {
	case Left:
		d.direction = Down
	case Right:
		d.direction = Up
	case Down:
		d.direction = Right
	case Up:
		d.direction = Left
	}
}

func (d *Direction) right() {
	switch d.direction {
	case Left:
		d.direction = Up
	case Right:
		d.direction = Down
	case Down:
		d.direction = Left
	case Up:
		d.direction = Right
	}
}

type DirectionType = int

const (
	Left DirectionType = iota
	Right
	Down
	Up
)

func (b *Board) getDirectionValue() int {
	switch b.curDirection.direction {
	case Left:
		return 2
	case Right:
		return 0
	case Down:
		return 1
	case Up:
		return 3
	default:
		panic("unhandled")
	}
}

func (b *Board) left() {
	b.curDirection.left()
}

func (b *Board) right() {
	b.curDirection.right()
}

// Return whether is blocked
func (b *Board) move() bool {
	p := b.board[b.curRow][b.curCol]
	switch b.curDirection.direction {
	case Left:
		if p.left == nil {
			if b.board[b.curRow][b.curCol-1].posType == Wall {
				return true
			}
			b.curCol--
			return false
		} else {
			left := p.left
			if b.board[left.row][left.col].posType == Wall {
				return true
			}
			b.curCol = left.col
			return false
		}
	case Right:
		if p.right == nil {
			if b.board[b.curRow][b.curCol+1].posType == Wall {
				return true
			}
			b.curCol++
			return false
		} else {
			right := p.right
			if b.board[right.row][right.col].posType == Wall {
				return true
			}
			b.curCol = right.col
			return false
		}
	case Down:
		if p.down == nil {
			if b.board[b.curRow+1][b.curCol].posType == Wall {
				return true
			}
			b.curRow++
			return false
		} else {
			down := p.down
			if b.board[down.row][down.col].posType == Wall {
				return true
			}
			b.curRow = down.row
			return false
		}
	case Up:
		if p.up == nil {
			if b.board[b.curRow-1][b.curCol].posType == Wall {
				return true
			}
			b.curRow--
			return false
		} else {
			up := p.up
			if b.board[up.row][up.col].posType == Wall {
				return true
			}
			b.curRow = up.row
			return false
		}
	default:
		panic("unhandled case")
	}
}

func (b *Board) print() {
	fmt.Println("----------Board----------")
	for row := 0; row < b.maxRow; row++ {
		for col := 0; col < b.maxCol; col++ {
			if row == b.curRow && col == b.curCol {
				switch b.curDirection.direction {
				case Left:
					fmt.Print("<")
				case Right:
					fmt.Print(">")
				case Down:
					fmt.Print("v")
				case Up:
					fmt.Print("^")
				}
				continue
			}

			switch b.board[row][col].posType {
			case Empty:
				fmt.Print(" ")
			case Open:
				fmt.Print(".")
			case Wall:
				fmt.Print("#")
			}
		}
		fmt.Println()
	}
}

func toBoard(lines []string) Board {
	maxRow := len(lines)
	maxCol := 0
	for _, line := range lines {
		if len(line) > maxCol {
			maxCol = len(line)
		}
	}

	board := make([][]Position, maxRow)
	for row := 0; row < maxRow; row++ {
		board[row] = make([]Position, maxCol)
		for col := 0; col < maxCol; col++ {
			board[row][col] = Position{row: row, col: col}
		}
	}

	for row, line := range lines {
		runes := []rune(line)
		for col, rune := range runes {
			switch rune {
			case '.':
				board[row][col].posType = Open
			case '#':
				board[row][col].posType = Wall
			}
		}
	}

	curCol := 0
	curRow := 0
	for col := 0; col < maxCol; col++ {
		if board[0][col].posType == Open {
			curCol = col
			break
		}
	}

	for row, positions := range board {
		startingCol := -1
		for col := 0; col < maxCol; col++ {
			if positions[col].posType != Empty {
				startingCol = col
				break
			}
		}

		endingCol := -1
		for col := maxCol - 1; col >= 0; col-- {
			if positions[col].posType != Empty {
				endingCol = col
				break
			}
		}

		board[row][startingCol].left = &board[row][endingCol]
		board[row][endingCol].right = &board[row][startingCol]
	}

	for col := 0; col < maxCol; col++ {
		startingRow := -1
		for row := 0; row < maxRow; row++ {
			if board[row][col].posType != Empty {
				startingRow = row
				break
			}
		}

		endingRow := -1
		for row := maxRow - 1; row >= 0; row-- {
			if board[row][col].posType != Empty {
				endingRow = row
				break
			}
		}

		board[startingRow][col].up = &board[endingRow][col]
		board[endingRow][col].down = &board[startingRow][col]
	}

	return Board{
		board:  board,
		maxRow: maxRow,
		maxCol: maxCol,
		curRow: curRow,
		curCol: curCol,
		curDirection: &Direction{
			direction: Right,
		},
	}
}

type Position struct {
	row     int
	col     int
	posType Type
	left    *Position
	right   *Position
	up      *Position
	down    *Position
}

type Type int

const (
	Empty Type = iota
	Open
	Wall
)

type Step struct {
	isMove bool

	move int
	turn DirectionType
}

func toSteps(s string) []Step {
	var steps []Step
	runes := []rune(s)
	for i := 0; i < len(runes); i++ {
		if runes[i] == 'R' {
			steps = append(steps, Step{turn: Right})
		} else if runes[i] == 'L' {
			steps = append(steps, Step{turn: Left})
		}

		n := toDigit(runes[i])
		i++
		for {
			if i == len(s) {
				steps = append(steps, Step{isMove: true, move: n})
				break
			}
			if runes[i] == 'L' || runes[i] == 'R' {
				i--
				steps = append(steps, Step{isMove: true, move: n})
				break
			}
			n = n*10 + toDigit(runes[i])
			i++
		}
	}
	return steps
}

func toDigit(r rune) int {
	s := string(r)
	i, _ := strconv.Atoi(s)
	return i
}

func fs2(input io.Reader, cubeSize int) (int, error) {
	scanner := bufio.NewScanner(input)
	var lines []string
	for scanner.Scan() {
		line := scanner.Text()
		lines = append(lines, line)
	}

	board := toBoard(lines[:len(lines)-2])

	steps := toSteps(lines[len(lines)-1])

	for _, step := range steps {
		if step.isMove {
			for i := 0; i < step.move; i++ {
				isBlocked := board.move()
				if isBlocked {
					break
				}
				//board.print()
			}
		} else {
			if step.turn == Left {
				board.left()
			} else {
				board.right()
			}
			//board.print()
		}
	}

	res := (board.curRow+1)*1000 + (board.curCol+1)*4 + board.getDirectionValue()
	return res, nil
}

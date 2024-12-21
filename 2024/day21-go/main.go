package main

import (
	"fmt"
	"io"

	"github.com/teivah/go-aoc"
)

type instruction struct {
	isDir bool
	dir   aoc.Direction
	isA   bool
}

func stringInstructions(instructions []instruction) string {
	s := ""
	for _, ins := range instructions {
		s += ins.String()
	}
	return s
}

func (i instruction) String() string {
	if i.isDir {
		switch i.dir {
		default:
			panic(i.dir)
		case aoc.Up:
			return "^"
		case aoc.Down:
			return "v"
		case aoc.Left:
			return "<"
		case aoc.Right:
			return ">"
		}
	}
	return "A"
}

type button struct {
	isDigit     bool
	digit       int
	isA         bool
	isDirection bool
	direction   aoc.Direction
}

func fs1(input io.Reader) int {
	codes := aoc.ReaderToStrings(input)
	numKeypad := formatNumericKeypad()
	robotA := getPosition(button{isA: true}, numKeypad)
	for _, code := range codes {
		buttons := expectedButtons(code)
		ins := getInstructions(numKeypad, buttons, robotA)
		fmt.Println(code, stringInstructions(ins))
	}
	return 42
}

func getInstructions(board aoc.Board[button], buttons []button, from aoc.Position) []instruction {
	var instructions []instruction
	cur := from
	for _, b := range buttons {
		row, col := delta(cur, b, board)
		ins := instructionsFromDelta(cur, board, row, col)
		instructions = append(instructions, ins...)
		cur = getPosition(b, board)
	}
	return instructions
}

func instructionsFromDelta(pos aoc.Position, board aoc.Board[button], row, col int) []instruction {
	var instructions []instruction
	cur := pos
	for row != 0 {
		if row < 0 {
			p := cur.Move(aoc.Up, 1)
			if contains(board, p) {
				cur = p
				instructions = append(instructions, instruction{isDir: true, dir: aoc.Up})
			} else {
				if col < 0 {
					cur = cur.Move(aoc.Left, 1)
					instructions = append(instructions, instruction{isDir: true, dir: aoc.Left})
				} else if col > 0 {
					cur = cur.Move(aoc.Right, 1)
					instructions = append(instructions, instruction{isDir: true, dir: aoc.Right})
				} else {
					panic("unknown")
				}
			}
			row++
		} else {
			p := cur.Move(aoc.Down, 1)
			if contains(board, p) {
				cur = p
				instructions = append(instructions, instruction{isDir: true, dir: aoc.Down})
			} else {
				if col < 0 {
					cur = cur.Move(aoc.Left, 1)
					instructions = append(instructions, instruction{isDir: true, dir: aoc.Left})
				} else if col > 0 {
					cur = cur.Move(aoc.Right, 1)
					instructions = append(instructions, instruction{isDir: true, dir: aoc.Right})
				} else {
					panic("unknown")
				}
			}
			row--
		}
	}

	for col != 0 {
		if col < 0 {
			cur = cur.Move(aoc.Left, 1)
			instructions = append(instructions, instruction{isDir: true, dir: aoc.Left})
			col++
		} else {
			cur = cur.Move(aoc.Right, 1)
			instructions = append(instructions, instruction{isDir: true, dir: aoc.Right})
			col--
		}
	}
	return append(instructions, instruction{isA: true})
}

func contains(board aoc.Board[button], pos aoc.Position) bool {
	if !board.Contains(pos) {
		return false
	}
	return board.Get(pos) != button{}
}

func expectedButtons(s string) []button {
	var buttons []button
	for _, c := range s {
		if c == 'A' {
			buttons = append(buttons, button{isA: true})
			continue
		}
		buttons = append(buttons, button{isDigit: true, digit: aoc.RuneToInt(c)})
	}
	return buttons
}

func getPosition(search button, board aoc.Board[button]) aoc.Position {
	for p, b := range board.Positions {
		if b == search {
			return p
		}
	}
	panic(search)
}

func delta(pos aoc.Position, to button, board aoc.Board[button]) (row, col int) {
	toPos := getPosition(to, board)
	return toPos.Row - pos.Row, toPos.Col - pos.Col
}

func formatNumericKeypad() aoc.Board[button] {
	elements := []aoc.BoardElement[button]{
		{T: button{isDigit: true, digit: 7}, Pos: aoc.NewPosition(0, 0)},
		{T: button{isDigit: true, digit: 8}, Pos: aoc.NewPosition(0, 1)},
		{T: button{isDigit: true, digit: 9}, Pos: aoc.NewPosition(0, 2)},
		{T: button{isDigit: true, digit: 4}, Pos: aoc.NewPosition(1, 0)},
		{T: button{isDigit: true, digit: 5}, Pos: aoc.NewPosition(1, 1)},
		{T: button{isDigit: true, digit: 6}, Pos: aoc.NewPosition(1, 2)},
		{T: button{isDigit: true, digit: 1}, Pos: aoc.NewPosition(2, 0)},
		{T: button{isDigit: true, digit: 2}, Pos: aoc.NewPosition(2, 1)},
		{T: button{isDigit: true, digit: 3}, Pos: aoc.NewPosition(2, 2)},
		{T: button{isDigit: true, digit: 0}, Pos: aoc.NewPosition(3, 1)},
		{T: button{isA: true}, Pos: aoc.NewPosition(3, 2)},
	}
	return aoc.NewBoardFromElements(elements, 4, 3)
}

func formatDirectionalKeypad() aoc.Board[button] {
	elements := []aoc.BoardElement[button]{
		{T: button{isDirection: true, direction: aoc.Up}, Pos: aoc.NewPosition(0, 1)},
		{T: button{isA: true}, Pos: aoc.NewPosition(0, 2)},
		{T: button{isDirection: true, direction: aoc.Up}, Pos: aoc.NewPosition(1, 0)},
		{T: button{isDirection: true, direction: aoc.Up}, Pos: aoc.NewPosition(1, 1)},
		{T: button{isDirection: true, direction: aoc.Up}, Pos: aoc.NewPosition(1, 2)},
	}
	return aoc.NewBoardFromElements(elements, 2, 3)
}

func fs2(input io.Reader) int {
	_ = aoc.ReaderToStrings(input)
	return 42
}

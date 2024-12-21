package main

import (
	"fmt"
	"io"
	"math"

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
	dirKeypad := formatDirectionalKeypad()
	for _, code := range codes {
		buttons := expectedButtons(code)
		insRobotA := getInstructions(numKeypad, buttons, robotA)

		robotB := getPosition(button{isA: true}, dirKeypad)
		insRobotB := directionalRobotInstructions(robotB, dirKeypad, insRobotA)
		fmt.Println(insRobotB)
	}
	return 42
}

func directionalRobotInstructions(pos aoc.Position, board aoc.Board[button], instructions []map[instruction]int) []map[instruction]int {
	cur := pos
	var res []map[instruction]int
	for _, set := range instructions {
		for len(set) != 0 {
			minDistance := math.MaxInt
			var bestButton button
			var best instruction
			var bestPosition aoc.Position
			for ins := range set {
				if len(set) > 1 && ins.isA {
					continue
				}
				var b button
				if ins.isA {
					b = button{isA: true}
				} else {
					b = button{isDirection: true, direction: ins.dir}
				}
				dst := getPosition(b, board)
				v := cur.Manhattan(dst)
				if v < minDistance {
					minDistance = v
					bestButton = b
					best = ins
					bestPosition = dst
				}
				minDistance = min(minDistance, cur.Manhattan(dst))
			}
			if minDistance == math.MaxInt {
				panic("not found")
			}
			ins := getInstructions(board, []button{bestButton}, cur)
			res = append(res, ins...)
			delete(set, best)
			cur = bestPosition
		}
	}
	return res
}

func getInstructions(board aoc.Board[button], buttons []button, from aoc.Position) []map[instruction]int {
	var instructions []map[instruction]int
	cur := from
	for _, b := range buttons {
		row, col := delta(cur, b, board)
		ins := instructionsFromDelta(cur, board, row, col)
		instructions = append(instructions, ins)
		cur = getPosition(b, board)
	}
	return instructions
}

func instructionsFromDelta(pos aoc.Position, board aoc.Board[button], row, col int) map[instruction]int {
	instructions := make(map[instruction]int)
	cur := pos
	for row != 0 {
		if row < 0 {
			p := cur.Move(aoc.Up, 1)
			if contains(board, p) {
				cur = p
				instructions[instruction{isDir: true, dir: aoc.Up}]++
			} else {
				if col < 0 {
					cur = cur.Move(aoc.Left, 1)
					instructions[instruction{isDir: true, dir: aoc.Left}]++
				} else if col > 0 {
					cur = cur.Move(aoc.Right, 1)
					instructions[instruction{isDir: true, dir: aoc.Right}]++
				} else {
					panic("unknown")
				}
			}
			row++
		} else {
			p := cur.Move(aoc.Down, 1)
			if contains(board, p) {
				cur = p
				instructions[instruction{isDir: true, dir: aoc.Down}]++
			} else {
				if col < 0 {
					cur = cur.Move(aoc.Left, 1)
					instructions[instruction{isDir: true, dir: aoc.Left}]++
				} else if col > 0 {
					cur = cur.Move(aoc.Right, 1)
					instructions[instruction{isDir: true, dir: aoc.Right}]++
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
			instructions[instruction{isDir: true, dir: aoc.Left}]++
			col++
		} else {
			cur = cur.Move(aoc.Right, 1)
			instructions[instruction{isDir: true, dir: aoc.Right}]++
			col--
		}
	}
	instructions[instruction{isA: true}]++
	return instructions
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
		{T: button{isDirection: true, direction: aoc.Left}, Pos: aoc.NewPosition(1, 0)},
		{T: button{isDirection: true, direction: aoc.Down}, Pos: aoc.NewPosition(1, 1)},
		{T: button{isDirection: true, direction: aoc.Right}, Pos: aoc.NewPosition(1, 2)},
	}
	return aoc.NewBoardFromElements(elements, 2, 3)
}

func fs2(input io.Reader) int {
	_ = aoc.ReaderToStrings(input)
	return 42
}

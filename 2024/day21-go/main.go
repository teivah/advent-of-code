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

var buttonA = button{isA: true}

// <vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A
// map[A:1 v:1 <:2] map[A:1 ^:1 >:2] map[A:1] map[A:1 v:1 <:1] map[A:1 <:1] map[A:1 ^:1 >:2] map[A:1] map[A:1 v:1] map[A:1] map[A:1 ^:1 <:1] map[A:1 >:1] map[A:1 v:1 <:1] map[A:1 ^:1 >:1] map[A:1] map[A:1 <:1] map[A:1 >:1] map[A:1 v:1 <:1] map[A:1 <:1] map[A:1 ^:1 >:2] map[A:1] map[A:1] map[A:1 <:1] map[A:1 v:1 >:1] map[A:1 ^:1]]
func fs1(input io.Reader) int {
	codes := aoc.ReaderToStrings(input)
	numKeypad := formatNumericKeypad()
	robotA := numKeypad.buttons[buttonA]
	dirKeypad := formatDirectionalKeypad()
	res := 0
	for _, code := range codes {
		buttons := expectedButtons(code)

		insRobotA := getInstructions(numKeypad, buttons, robotA)

		robotB := dirKeypad.buttons[buttonA]
		insRobotB := directionalRobotInstructions(robotB, dirKeypad, insRobotA)

		robotC := dirKeypad.buttons[buttonA]
		insRobotC := directionalRobotInstructions(robotC, dirKeypad, insRobotB)
		fmt.Println(insRobotC)

		iCode := aoc.StringToInt(code[:len(code)-1])
		res += iCode * count(insRobotC)
		fmt.Println(count(insRobotC), iCode)
	}
	return res
}

func count(ins []map[instruction]int) int {
	res := 0
	for _, i := range ins {
		for _, c := range i {
			res += c
		}
	}
	return res
}

func directionalRobotInstructions(pos aoc.Position, board Board, instructions []map[instruction]int) []map[instruction]int {
	cur := pos
	var res []map[instruction]int
	for _, set := range instructions {
		mult := false
		for _, v := range set {
			if v > 1 {
				mult = true
			}
		}
		_ = mult
		for len(set) != 0 {
			minDistance := math.MaxInt
			var bestButton button
			var best instruction
			var bestCount int
			var bestPosition aoc.Position
			for ins, count := range set {
				if len(set) > 1 && ins.isA {
					continue
				}
				var b button
				if ins.isA {
					b = buttonA
				} else {
					b = button{isDirection: true, direction: ins.dir}
				}
				dst := board.buttons[b]
				v := cur.Manhattan(dst)
				if v < minDistance {
					minDistance = v
					bestButton = b
					best = ins
					bestPosition = dst
					bestCount = count
				}
				minDistance = min(minDistance, cur.Manhattan(dst))
			}
			if minDistance == math.MaxInt {
				panic("not found")
			}
			ins := getInstructions(board, []button{bestButton}, cur)
			res = append(res, ins...)
			if bestCount == 1 {
				delete(set, best)
			} else {
				set[best]--
			}
			cur = bestPosition
		}
	}
	return res
}

func getInstructions(board Board, buttons []button, from aoc.Position) []map[instruction]int {
	var instructions []map[instruction]int
	cur := from
	for _, b := range buttons {
		row, col := delta(cur, b, board)
		ins := instructionsFromDelta(cur, board, row, col)
		instructions = append(instructions, ins)
		cur = board.buttons[b]
	}
	return instructions
}

func instructionsFromDelta(pos aoc.Position, board Board, row, col int) map[instruction]int {
	instructions := make(map[instruction]int)
	cur := pos
	for row != 0 {
		if row < 0 {
			p := cur.Move(aoc.Up, 1)
			if board.contains(p) {
				cur = p
				instructions[instruction{isDir: true, dir: aoc.Up}]++
				row++
			} else {
				if col < 0 {
					cur = cur.Move(aoc.Left, 1)
					instructions[instruction{isDir: true, dir: aoc.Left}]++
					col++
				} else if col > 0 {
					cur = cur.Move(aoc.Right, 1)
					instructions[instruction{isDir: true, dir: aoc.Right}]++
					col--
				} else {
					panic("unknown")
				}
			}
		} else {
			p := cur.Move(aoc.Down, 1)
			if board.contains(p) {
				cur = p
				instructions[instruction{isDir: true, dir: aoc.Down}]++
				row--
			} else {
				if col < 0 {
					cur = cur.Move(aoc.Left, 1)
					instructions[instruction{isDir: true, dir: aoc.Left}]++
					col++
				} else if col > 0 {
					cur = cur.Move(aoc.Right, 1)
					instructions[instruction{isDir: true, dir: aoc.Right}]++
					col--
				} else {
					panic("unknown")
				}
			}
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

func expectedButtons(s string) []button {
	var buttons []button
	for _, c := range s {
		if c == 'A' {
			buttons = append(buttons, buttonA)
			continue
		}
		buttons = append(buttons, button{isDigit: true, digit: aoc.RuneToInt(c)})
	}
	return buttons
}

func delta(pos aoc.Position, to button, board Board) (row, col int) {
	toPos := board.buttons[to]
	return toPos.Row - pos.Row, toPos.Col - pos.Col
}

func formatNumericKeypad() Board {
	m := map[button]aoc.Position{
		button{isDigit: true, digit: 7}: aoc.NewPosition(0, 0),
		button{isDigit: true, digit: 8}: aoc.NewPosition(0, 1),
		button{isDigit: true, digit: 9}: aoc.NewPosition(0, 2),
		button{isDigit: true, digit: 4}: aoc.NewPosition(1, 0),
		button{isDigit: true, digit: 5}: aoc.NewPosition(1, 1),
		button{isDigit: true, digit: 6}: aoc.NewPosition(1, 2),
		button{isDigit: true, digit: 1}: aoc.NewPosition(2, 0),
		button{isDigit: true, digit: 2}: aoc.NewPosition(2, 1),
		button{isDigit: true, digit: 3}: aoc.NewPosition(2, 2),
		button{isDigit: true, digit: 0}: aoc.NewPosition(3, 1),
		buttonA:                         aoc.NewPosition(3, 2),
	}
	return newBoard(m)
}

type Board struct {
	buttons   map[button]aoc.Position
	positions map[aoc.Position]button
}

func newBoard(buttons map[button]aoc.Position) Board {
	positions := make(map[aoc.Position]button, len(buttons))
	for b, p := range buttons {
		positions[p] = b
	}
	return Board{
		buttons:   buttons,
		positions: positions,
	}
}

func (b Board) contains(pos aoc.Position) bool {
	_, ok := b.positions[pos]
	return ok
}

func formatDirectionalKeypad() Board {
	m := map[button]aoc.Position{
		button{isDirection: true, direction: aoc.Up}: aoc.NewPosition(0, 1),
		buttonA: aoc.NewPosition(0, 2),
		button{isDirection: true, direction: aoc.Left}:  aoc.NewPosition(1, 0),
		button{isDirection: true, direction: aoc.Down}:  aoc.NewPosition(1, 1),
		button{isDirection: true, direction: aoc.Right}: aoc.NewPosition(1, 2),
	}
	return newBoard(m)
}

func fs2(input io.Reader) int {
	_ = aoc.ReaderToStrings(input)
	return 42
}

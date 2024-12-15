package main

import (
	"fmt"
	"io"
	"sort"

	"github.com/teivah/go-aoc"
)

type Cell struct {
	wall  bool
	empty bool
	box   bool
	robot bool
}

func fs1(input io.Reader) int {
	lines := aoc.StringGroups(aoc.ReaderToStrings(input))
	var cur aoc.Position
	board := aoc.ParseBoard(lines[0], func(r rune, pos aoc.Position) Cell {
		switch r {
		default:
			panic(r)
		case '#':
			return Cell{wall: true}
		case '.':
			return Cell{empty: true}
		case 'O':
			return Cell{box: true}
		case '@':
			cur = pos
			return Cell{robot: true}
		}
	})

	for _, instructions := range lines[1] {
		for _, instruction := range instructions {
			switch instruction {
			default:
				panic(instruction)
			case '<':
				cur = move(cur, aoc.Left, board)
			case '>':
				cur = move(cur, aoc.Right, board)
			case '^':
				cur = move(cur, aoc.Up, board)
			case 'v':
				cur = move(cur, aoc.Down, board)
			}
		}
	}

	res := 0
	for pos, cell := range board.Positions {
		if cell.box {
			res += 100*pos.Row + pos.Col
		}
	}
	return res
}

func display(board aoc.Board[Cell2]) {
	fmt.Println(board.String(func(cell Cell2) rune {
		switch {
		default:
			panic(cell)
		case cell.robot:
			return '@'
		case cell.wall:
			return '#'
		case cell.empty:
			return '.'
		case cell.boxStart:
			return '['
		case cell.boxEnd:
			return ']'
		}
	}, 'x'))
}

func move(pos aoc.Position, dir aoc.Direction, board aoc.Board[Cell]) aoc.Position {
	next := pos.Move(dir, 1)
	e := board.Get(next)
	switch {
	default:
		panic(e)
	case e.box:
		p := next
		for {
			p = p.Move(dir, 1)
			e = board.Get(p)
			switch {
			default:
				panic(e)
			case e.box:
				continue
			case e.wall:
				return pos
			case e.empty:
				board.Positions[p] = Cell{box: true}
				board.Positions[pos] = Cell{empty: true}
				board.Positions[next] = Cell{robot: true}
				return next
			}
		}
	case e.wall:
		return pos
	case e.empty:
		board.Positions[pos] = Cell{empty: true}
		board.Positions[next] = Cell{robot: true}
		return next
	}
}

type Cell2 struct {
	wall     bool
	empty    bool
	boxStart bool
	boxEnd   bool
	robot    bool
}

func fs2(input io.Reader) int {
	lines := aoc.StringGroups(aoc.ReaderToStrings(input))

	var cur aoc.Position
	board := aoc.ParseBoard(board2(lines[0]), func(r rune, pos aoc.Position) Cell2 {
		switch r {
		default:
			panic(r)
		case '#':
			return Cell2{wall: true}
		case '.':
			return Cell2{empty: true}
		case '[':
			return Cell2{boxStart: true}
		case ']':
			return Cell2{boxEnd: true}
		case '@':
			cur = pos
			return Cell2{robot: true}
		}
	})

	//display(board)
	i := -1
	for _, instructions := range lines[1] {
		for _, instruction := range instructions {
			i++
			//fmt.Println(i, string(instruction))
			switch instruction {
			default:
				panic(instruction)
			case '<':
				cur, _ = move2(map[aoc.Position]bool{}, cur, aoc.Left, board)
			case '>':
				cur, _ = move2(map[aoc.Position]bool{}, cur, aoc.Right, board)
			case '^':
				cur, _ = move2(map[aoc.Position]bool{}, cur, aoc.Up, board)
			case 'v':
				cur, _ = move2(map[aoc.Position]bool{}, cur, aoc.Down, board)
			}
			//display(board)
		}
	}

	res := 0
	for pos, cell := range board.Positions {
		if cell.boxStart {
			res += 100*pos.Row + pos.Col
		}
	}
	return res
}

func move2(set map[aoc.Position]bool, pos aoc.Position, dir aoc.Direction, board aoc.Board[Cell2]) (aoc.Position, bool) {
	next := pos.Move(dir, 1)
	e := board.Get(next)
	switch {
	default:
		panic(e)
	case e.wall:
		return pos, false
	case e.boxStart, e.boxEnd:
		set[next] = true
		switch dir {
		default:
			panic(dir)
		case aoc.Left, aoc.Right:
			if _, shouldMove := move2(set, pos.Move(dir, 1), dir, board); !shouldMove {
				return pos, false
			}
			if board.Get(pos).robot {
				shift(set, map[aoc.Position]bool{}, 0, pos, dir, board)
				return pos.Move(dir, 1), true
			} else {
				return pos, true
			}
		case aoc.Up, aoc.Down:
			if _, shouldMove := move2(set, pos.Move(dir, 1), dir, board); !shouldMove {
				return pos, false
			}
			if e.boxStart {
				set[next.Move(aoc.Right, 1)] = true
				if _, shouldMove := move2(set, pos.Move(dir, 1).Move(aoc.Right, 1), dir, board); !shouldMove {
					return pos, false
				}
			} else {
				set[next.Move(aoc.Left, 1)] = true
				if _, shouldMove := move2(set, pos.Move(dir, 1).Move(aoc.Left, 1), dir, board); !shouldMove {
					return pos, false
				}
			}
			if board.Get(pos).robot {
				shift(set, map[aoc.Position]bool{}, 0, pos, dir, board)
				return pos.Move(dir, 1), true
			} else {
				return pos, true
			}
		}
	case e.empty:
		if board.Get(pos).robot {
			board.Positions[pos] = Cell2{empty: true}
			board.Positions[next] = Cell2{robot: true}
			return next, true
		} else {
			return pos, true
		}
	}
}

func shift(set, visited map[aoc.Position]bool, count int, pos aoc.Position, dir aoc.Direction, board aoc.Board[Cell2]) {
	positions := aoc.MapKeysToSlice(set)
	sort.Slice(positions, func(i, j int) bool {
		a := positions[i]
		b := positions[j]
		switch dir {
		default:
			panic(dir)
		case aoc.Left:
			return a.Col < b.Col
		case aoc.Right:
			return a.Col > b.Col
		case aoc.Up:
			return a.Row < b.Row
		case aoc.Down:
			return a.Row > b.Row
		}
	})
	for _, p := range positions {
		board.Positions[p.Move(dir, 1)] = board.Positions[p]
		board.Positions[p] = Cell2{empty: true}
	}
	board.Positions[pos.Move(dir, 1)] = board.Positions[pos]
	board.Positions[pos] = Cell2{empty: true}
}

func board2(lines []string) []string {
	var res []string
	for _, line := range lines {
		cur := ""
		for _, r := range line {
			switch r {
			default:
				panic(r)
			case '#':
				cur += "##"
			case '.':
				cur += ".."
			case 'O':
				cur += "[]"
			case '@':
				cur += "@."
			}
		}
		res = append(res, cur)
	}
	return res
}

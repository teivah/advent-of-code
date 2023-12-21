package main

import (
	"io"

	aoc "github.com/teivah/advent-of-code"
)

type squareType int

const (
	empty squareType = iota
	rock
)

func fs1(input io.Reader, iterations int) int {
	var start aoc.Position
	board := aoc.ParseBoard(aoc.ReaderToStrings(input), func(r rune, pos aoc.Position) squareType {
		switch r {
		case '.':
			return empty
		case '#':
			return rock
		case 'S':
			start = pos
			return empty
		default:
			panic(r)
		}
	})

	q := []aoc.Position{start}
	for it := 0; it < iterations; it++ {
		length := len(q)
		positions := make(map[aoc.Position]struct{})
		for i := 0; i < length; i++ {
			p := q[0]
			q = q[1:]

			moves := []aoc.Direction{aoc.Up, aoc.Down, aoc.Left, aoc.Right}
			for _, move := range moves {
				t := p.Move(move, 1)
				if board.Contains(t) && board.Get(t) != rock {
					if _, exists := positions[t]; exists {
						continue
					}
					positions[t] = struct{}{}
					q = append(q, t)
				}
			}
		}
	}

	return len(q)
}

func fs2(input io.Reader, iterations int) int {
	var start aoc.Position
	board := aoc.ParseBoard(aoc.ReaderToStrings(input), func(r rune, pos aoc.Position) squareType {
		switch r {
		case '.':
			return empty
		case '#':
			return rock
		case 'S':
			start = pos
			return empty
		default:
			panic(r)
		}
	})

	type position struct {
		boardRow int
		boardCol int
		position aoc.Position
	}

	q := []position{
		{position: start},
	}
	for it := 0; it < iterations; it++ {
		length := len(q)
		positions := make(map[position]struct{})
		for i := 0; i < length; i++ {
			p := q[0]
			q = q[1:]

			moves := []aoc.Direction{aoc.Up, aoc.Down, aoc.Left, aoc.Right}
			for _, move := range moves {
				p := p

				t := p.position.Move(move, 1)
				if t.Row < 0 {
					p.boardRow--
					t.Row = board.MaxRows - 1
				} else if t.Row == board.MaxRows {
					p.boardRow++
					t.Row = 0
				} else if t.Col < 0 {
					p.boardCol--
					t.Col = board.MaxCols - 1
				} else if t.Col == board.MaxCols {
					p.boardCol++
					t.Col = 0
				}
				p.position = t

				if board.Get(t) == rock {
					continue
				}

				if _, exists := positions[p]; exists {
					continue
				}
				positions[p] = struct{}{}
				q = append(q, p)
			}
		}
	}

	return len(q)
}

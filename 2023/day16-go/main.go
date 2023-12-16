package main

import (
	"context"
	"io"
	"sync"

	aoc "github.com/teivah/advent-of-code"
	"golang.org/x/sync/errgroup"
)

type TileType int

const (
	empty TileType = iota
	mirrorRight
	mirrorLeft
	splitterVertical
	splitterHorizontal
)

type Beam struct {
	pos aoc.Position
	dir aoc.Direction
}

func fs1(input io.Reader) int {
	board := parse(aoc.ReaderToStrings(input))
	return fire(board, Beam{
		pos: aoc.Position{Row: 0, Col: 0},
		dir: aoc.Right,
	})
}

func fire(board map[aoc.Position]TileType, first Beam) int {
	cache := make(map[Beam]struct{})
	energized := make(map[aoc.Position]struct{})
	q := []Beam{first}

	for len(q) != 0 {
		beam := q[0]
		q = q[1:]

		if _, exists := cache[beam]; exists {
			continue
		}

		t, exists := board[beam.pos]
		if !exists {
			continue
		}
		cache[beam] = struct{}{}
		energized[beam.pos] = struct{}{}

		switch t {
		case empty:
			beam.pos = beam.pos.Move(beam.dir, 1)
			q = append(q, beam)
		case mirrorRight:
			switch beam.dir {
			case aoc.Left:
				beam.dir = aoc.Down
			case aoc.Right:
				beam.dir = aoc.Up
			case aoc.Up:
				beam.dir = aoc.Right
			case aoc.Down:
				beam.dir = aoc.Left
			default:
				panic(beam.dir)
			}
			beam.pos = beam.pos.Move(beam.dir, 1)
			q = append(q, beam)
		case mirrorLeft:
			switch beam.dir {
			case aoc.Left:
				beam.dir = aoc.Up
			case aoc.Right:
				beam.dir = aoc.Down
			case aoc.Up:
				beam.dir = aoc.Left
			case aoc.Down:
				beam.dir = aoc.Right
			default:
				panic(beam.dir)
			}
			beam.pos = beam.pos.Move(beam.dir, 1)
			q = append(q, beam)
		case splitterVertical:
			switch beam.dir {
			case aoc.Left, aoc.Right:
				q = append(q, Beam{
					pos: beam.pos.Move(aoc.Up, 1),
					dir: aoc.Up,
				})
				q = append(q, Beam{
					pos: beam.pos.Move(aoc.Down, 1),
					dir: aoc.Down,
				})
			case aoc.Up, aoc.Down:
				beam.pos = beam.pos.Move(beam.dir, 1)
				q = append(q, beam)
			}
		case splitterHorizontal:
			switch beam.dir {
			case aoc.Left, aoc.Right:
				beam.pos = beam.pos.Move(beam.dir, 1)
				q = append(q, beam)
			case aoc.Up, aoc.Down:
				q = append(q, Beam{
					pos: beam.pos.Move(aoc.Left, 1),
					dir: aoc.Left,
				})
				q = append(q, Beam{
					pos: beam.pos.Move(aoc.Right, 1),
					dir: aoc.Right,
				})
			}
		}
	}

	return len(energized)
}

func parse(lines []string) map[aoc.Position]TileType {
	board := make(map[aoc.Position]TileType)
	for row, line := range lines {
		for col, c := range line {
			pos := aoc.Position{Row: row, Col: col}
			switch c {
			case '.':
				board[pos] = empty
			case '/':
				board[pos] = mirrorRight
			case '\\':
				board[pos] = mirrorLeft
			case '|':
				board[pos] = splitterVertical
			case '-':
				board[pos] = splitterHorizontal
			default:
				panic(c)
			}
		}
	}
	return board
}

func fs2(input io.Reader) int {
	lines := aoc.ReaderToStrings(input)
	board := parse(lines)
	res := 0
	for row := 0; row < len(lines); row++ {
		res = max(res, fire(board, Beam{
			pos: aoc.Position{Row: row, Col: 0},
			dir: aoc.Right,
		}))
		res = max(res, fire(board, Beam{
			pos: aoc.Position{Row: row, Col: len(lines[0]) - 1},
			dir: aoc.Left,
		}))
	}
	for col := 0; col < len(lines[0]); col++ {
		res = max(res, fire(board, Beam{
			pos: aoc.Position{Row: 0, Col: col},
			dir: aoc.Down,
		}))
		res = max(res, fire(board, Beam{
			pos: aoc.Position{Row: len(lines) - 1, Col: col},
			dir: aoc.Up,
		}))
	}
	return res
}

func fs2Concurrency(input io.Reader) int {
	lines := aoc.ReaderToStrings(input)
	board := parse(lines)
	res := 0
	eg, _ := errgroup.WithContext(context.Background())
	mu := sync.Mutex{}

	updateMax := func(v int) {
		mu.Lock()
		defer mu.Unlock()
		res = max(v, res)
	}

	for row := 0; row < len(lines); row++ {
		row := row
		eg.Go(func() error {
			v := fire(board, Beam{
				pos: aoc.Position{Row: row, Col: 0},
				dir: aoc.Right,
			})
			updateMax(v)
			return nil
		})
		eg.Go(func() error {
			v := fire(board, Beam{
				pos: aoc.Position{Row: row, Col: len(lines[0]) - 1},
				dir: aoc.Left,
			})
			updateMax(v)
			return nil
		})
	}

	for col := 0; col < len(lines[0]); col++ {
		col := col
		eg.Go(func() error {
			v := fire(board, Beam{
				pos: aoc.Position{Row: 0, Col: col},
				dir: aoc.Down,
			})
			updateMax(v)
			return nil
		})
		eg.Go(func() error {
			v := fire(board, Beam{
				pos: aoc.Position{Row: len(lines) - 1, Col: col},
				dir: aoc.Up,
			})
			updateMax(v)
			return nil
		})
	}

	_ = eg.Wait()
	return res
}

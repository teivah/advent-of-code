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
	board, _, _ := aoc.ParseBoard[TileType](aoc.ReaderToStrings(input), toTileType)
	return fire(board, Beam{
		pos: aoc.Position{Row: 0, Col: 0},
		dir: aoc.Right,
	})
}

func toTileType(r rune) TileType {
	switch r {
	case '.':
		return empty
	case '/':
		return mirrorRight
	case '\\':
		return mirrorLeft
	case '|':
		return splitterVertical
	case '-':
		return splitterHorizontal
	default:
		panic(r)
	}
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

func fs2(input io.Reader) int {
	board, rows, cols := aoc.ParseBoard[TileType](aoc.ReaderToStrings(input), toTileType)
	res := 0
	for row := 0; row < rows; row++ {
		res = max(res, fire(board, Beam{
			pos: aoc.Position{Row: row, Col: 0},
			dir: aoc.Right,
		}))
		res = max(res, fire(board, Beam{
			pos: aoc.Position{Row: row, Col: cols - 1},
			dir: aoc.Left,
		}))
	}
	for col := 0; col < cols; col++ {
		res = max(res, fire(board, Beam{
			pos: aoc.Position{Row: 0, Col: col},
			dir: aoc.Down,
		}))
		res = max(res, fire(board, Beam{
			pos: aoc.Position{Row: rows - 1, Col: col},
			dir: aoc.Up,
		}))
	}
	return res
}

// Created for the sake of https://www.reddit.com/r/adventofcode/comments/18k1q32/2023_day16_part_12_golang/
func fs2Concurrency(input io.Reader) int {
	board, rows, cols := aoc.ParseBoard[TileType](aoc.ReaderToStrings(input), toTileType)
	res := 0
	eg, _ := errgroup.WithContext(context.Background())
	mu := sync.Mutex{}

	updateMax := func(v int) {
		mu.Lock()
		defer mu.Unlock()
		res = max(v, res)
	}

	for row := 0; row < rows; row++ {
		row := row
		eg.Go(func() error {
			updateMax(fire(board, Beam{
				pos: aoc.Position{Row: row, Col: 0},
				dir: aoc.Right,
			}))
			return nil
		})
		eg.Go(func() error {
			updateMax(fire(board, Beam{
				pos: aoc.Position{Row: row, Col: cols - 1},
				dir: aoc.Left,
			}))
			return nil
		})
	}

	for col := 0; col < cols; col++ {
		col := col
		eg.Go(func() error {
			updateMax(fire(board, Beam{
				pos: aoc.Position{Row: 0, Col: col},
				dir: aoc.Down,
			}))
			return nil
		})
		eg.Go(func() error {
			updateMax(fire(board, Beam{
				pos: aoc.Position{Row: rows - 1, Col: col},
				dir: aoc.Up,
			}))
			return nil
		})
	}

	_ = eg.Wait()
	return res
}

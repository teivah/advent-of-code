package main

import (
	"io"
	"math"

	"github.com/teivah/go-aoc"
)

type cell uint32

const (
	end cell = iota
	empty
	wall
)

func (c cell) isNonWall() bool {
	return c == empty || c == end
}

func fs1(input io.Reader) int {
	s := newSolver(input)
	for !s.q.IsEmpty() {
		s.step()
	}
	return s.best
}

func fs2(input io.Reader) int {
	s := newSolver(input)
	for !s.q.IsEmpty() {
		s.step()
	}
	return len(s.paths)
}

type state struct {
	loc   aoc.Location
	count int
	path  map[aoc.Position]bool
}

type solver struct {
	q       aoc.Heap[state]
	board   aoc.Board[cell]
	visited map[aoc.Location]int
	best    int
	paths   map[aoc.Position]bool
}

func newSolver(input io.Reader) *solver {
	var start aoc.Location
	board := aoc.ParseBoard(aoc.ReaderToStrings(input), func(r rune, pos aoc.Position) cell {
		switch r {
		default:
			panic(r)
		case 'S':
			start = aoc.NewLocation(pos.Row, pos.Col, aoc.Right)
			return empty
		case 'E':
			return end
		case '.':
			return empty
		case '#':
			return wall
		}
	})

	s := solver{
		q: aoc.NewHeap(func(a, b state) bool {
			return a.count < b.count
		}),
		board:   board,
		visited: make(map[aoc.Location]int),
		best:    math.MaxInt,
	}
	s.q.Push(state{
		loc:   start,
		count: 0,
		path:  map[aoc.Position]bool{start.Pos: true},
	})
	return &s
}

func (s *solver) step() {
	cur := s.q.Pop()

	if s.board.Get(cur.loc.Pos) == end {
		if cur.count < s.best {
			s.best = cur.count
			s.paths = cur.path
		} else if cur.count == s.best {
			for p := range cur.path {
				s.paths[p] = true
			}
		}
		return
	}

	if cur.count >= s.best {
		return
	}

	if c, ok := s.visited[cur.loc]; ok && c < cur.count {
		return
	}
	s.visited[cur.loc] = cur.count

	next := cur.loc.Straight(1)
	if s.board.Get(next.Pos).isNonWall() {
		path := aoc.MapCopy(cur.path)
		path[next.Pos] = true
		s.q.Push(state{
			loc:   next,
			count: cur.count + 1,
			path:  path,
		})
	}

	s.q.Push(state{
		loc:   cur.loc.Turn(aoc.Left, 0),
		count: cur.count + 1000,
		path:  cur.path,
	})

	s.q.Push(state{
		loc:   cur.loc.Turn(aoc.Right, 0),
		count: cur.count + 1000,
		path:  cur.path,
	})
}

package main

import (
	"io"
	"math"

	"github.com/teivah/go-aoc"
)

type Cell struct {
	end   bool
	empty bool
	wall  bool
}

func (c Cell) isNonWall() bool {
	return c.empty || c.end
}

func fs1(input io.Reader) int {
	var start aoc.Location
	board := aoc.ParseBoard(aoc.ReaderToStrings(input), func(r rune, pos aoc.Position) Cell {
		switch r {
		default:
			panic(r)
		case 'S':
			start = aoc.NewLocation(pos.Row, pos.Col, aoc.Right)
			return Cell{empty: true}
		case 'E':
			return Cell{end: true}
		case '.':
			return Cell{empty: true}
		case '#':
			return Cell{wall: true}
		}
	})

	s := Solver{
		q: aoc.NewHeap(func(a, b State) bool {
			return a.count < b.count
		}),
		board:   board,
		visited: make(map[aoc.Location]int),
		best:    math.MaxInt,
	}
	s.q.Push(State{
		loc:   start,
		count: 0,
		path:  map[aoc.Position]bool{start.Pos: true},
	})

	for !s.q.IsEmpty() {
		s.solve1()
	}
	return s.best
}

type State struct {
	loc   aoc.Location
	count int
	path  map[aoc.Position]bool
}

type Solver struct {
	q       aoc.Heap[State]
	board   aoc.Board[Cell]
	visited map[aoc.Location]int
	best    int
	paths   map[aoc.Position]bool
}

func (s *Solver) solve1() {
	cur := s.q.Pop()

	if s.board.Get(cur.loc.Pos).end {
		s.best = min(s.best, cur.count)
		return
	}

	if s.best != 0 && cur.count >= s.best {
		return
	}
	if c, ok := s.visited[cur.loc]; ok {
		if c <= cur.count {
			return
		}
	}
	s.visited[cur.loc] = cur.count

	// Straight
	next := cur.loc.Straight(1)
	pos := s.board.Get(next.Pos)
	if pos.isNonWall() {
		s.q.Push(State{
			loc:   next,
			count: cur.count + 1,
		})
	}

	// Left
	next = cur.loc.Turn(aoc.Left, 0)
	s.q.Push(State{
		loc:   next,
		count: cur.count + 1000,
	})

	// Right
	next = cur.loc.Turn(aoc.Right, 0)
	s.q.Push(State{
		loc:   next,
		count: cur.count + 1000,
	})
}

func fs2(input io.Reader) int {
	var start aoc.Location
	board := aoc.ParseBoard(aoc.ReaderToStrings(input), func(r rune, pos aoc.Position) Cell {
		switch r {
		default:
			panic(r)
		case 'S':
			start = aoc.NewLocation(pos.Row, pos.Col, aoc.Right)
			return Cell{empty: true}
		case 'E':
			return Cell{end: true}
		case '.':
			return Cell{empty: true}
		case '#':
			return Cell{wall: true}
		}
	})

	s := Solver{
		q: aoc.NewHeap(func(a, b State) bool {
			return a.count < b.count
		}),
		board:   board,
		visited: make(map[aoc.Location]int),
		best:    math.MaxInt,
	}
	s.q.Push(State{
		loc:   start,
		count: 0,
		path:  map[aoc.Position]bool{start.Pos: true},
	})

	for !s.q.IsEmpty() {
		s.solve2()
	}
	return len(s.paths)
}

func (s *Solver) solve2() {
	cur := s.q.Pop()

	if s.board.Get(cur.loc.Pos).end {
		if cur.count < s.best {
			s.best = cur.count
			s.paths = cur.path
		} else if cur.count == s.best {
			s.mergePaths(cur.path)
		}
		return
	}

	if s.best != 0 && cur.count >= s.best {
		return
	}
	if c, ok := s.visited[cur.loc]; ok {
		if c < cur.count {
			return
		}
	}
	s.visited[cur.loc] = cur.count

	// Straight
	next := cur.loc.Straight(1)
	pos := s.board.Get(next.Pos)
	if pos.isNonWall() {
		path := aoc.MapCopy(cur.path)
		path[next.Pos] = true
		s.q.Push(State{
			loc:   next,
			count: cur.count + 1,
			path:  path,
		})
	}

	// Left
	next = cur.loc.Turn(aoc.Left, 0)
	s.q.Push(State{
		loc:   next,
		count: cur.count + 1000,
		path:  cur.path,
	})

	// Right
	next = cur.loc.Turn(aoc.Right, 0)
	s.q.Push(State{
		loc:   next,
		count: cur.count + 1000,
		path:  cur.path,
	})
}

func (s *Solver) mergePaths(path map[aoc.Position]bool) {
	for p := range path {
		s.paths[p] = true
	}
}
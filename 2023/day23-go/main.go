package main

import (
	"fmt"
	"io"

	aoc "github.com/teivah/advent-of-code"
)

type trailType int

const (
	path trailType = iota
	forest
	slopeDown
	slopeRight
)

type Entry struct {
	loc   aoc.Location
	down  int
	right int
}

type State struct {
	Entry
	moves int
}

type Board struct {
	board       aoc.Board[trailType]
	downSlopes  map[aoc.Position]int
	rightSlopes map[aoc.Position]int
	moves       map[aoc.Location][]Destination
}

type Destination struct {
	loc   aoc.Location
	moves int
}

func fs1(input io.Reader) int {
	var (
		idxDown     = 0
		idxRight    = 0
		downSlopes  = make(map[aoc.Position]int)
		rightSlopes = make(map[aoc.Position]int)
	)
	b := aoc.ParseBoard(aoc.ReaderToStrings(input), func(r rune, pos aoc.Position) trailType {
		switch r {
		case '.':
			return path
		case '#':
			return forest
		case 'v':
			downSlopes[pos] = idxDown
			idxDown++
			return slopeDown
		case '>':
			rightSlopes[pos] = idxRight
			idxRight++
			return slopeRight
		default:
			panic(r)
		}
	})

	board := Board{
		board:       b,
		downSlopes:  downSlopes,
		rightSlopes: rightSlopes,
	}
	target := aoc.NewPosition(board.board.MaxRows-1, board.board.MaxCols-2)

	cache := make(map[Entry]struct{})

	q := []State{
		{
			Entry: Entry{
				loc: aoc.NewLocation(0, 1, aoc.Down),
			},
		},
	}

	best := 0

	for len(q) != 0 {
		s := q[0]
		q = q[1:]

		if s.loc.Pos == target {
			best = max(best, s.moves)
			continue
		}

		if _, exists := cache[s.Entry]; exists {
			continue
		}
		cache[s.Entry] = struct{}{}

		s.moves++
		if e2, exists := move(board, s, s.loc.Straight(1)); exists {
			q = append(q, e2)
		}
		if e2, exists := move(board, s, s.loc.Turn(aoc.Left, 1)); exists {
			q = append(q, e2)
		}
		if e2, exists := move(board, s, s.loc.Turn(aoc.Right, 1)); exists {
			q = append(q, e2)
		}
	}

	return best
}

func move(board Board, s State, target aoc.Location) (State, bool) {
	t := board.board.Get(target.Pos)
	switch t {
	case forest:
		return State{}, false
	case path:
		return State{
			Entry: Entry{
				loc: aoc.Location{
					Pos: target.Pos,
					Dir: target.Dir,
				},
				down:  s.down,
				right: s.right,
			},
			moves: s.moves,
		}, true
	case slopeRight:
		if target.Dir == aoc.Left {
			return State{}, false
		}

		loc := target.Move(aoc.Right, 1)
		right := s.right | 1<<board.rightSlopes[target.Pos]
		if right == s.right {
			return State{}, false
		}
		return State{
			Entry: Entry{
				loc: aoc.Location{
					Pos: loc.Pos,
					Dir: aoc.Right,
				},
				down:  s.down,
				right: right,
			},
			moves: s.moves + 1,
		}, true
	case slopeDown:
		if target.Dir == aoc.Up {
			return State{}, false
		}

		loc := target.Move(aoc.Down, 1)
		down := s.down | 1<<board.downSlopes[target.Pos]
		if down == s.down {
			return State{}, false
		}
		return State{
			Entry: Entry{
				loc: aoc.Location{
					Pos: loc.Pos,
					Dir: aoc.Down,
				},
				down:  down,
				right: s.right,
			},
			moves: s.moves + 1,
		}, true
	}
	panic("unhandled case")
}

func fs2(input io.Reader) int {
	var (
		idxDown     = 0
		idxRight    = 0
		downSlopes  = make(map[aoc.Position]int)
		rightSlopes = make(map[aoc.Position]int)
	)
	b := aoc.ParseBoard(aoc.ReaderToStrings(input), func(r rune, pos aoc.Position) trailType {
		switch r {
		case '.':
			return path
		case '#':
			return forest
		case 'v':
			downSlopes[pos] = idxDown
			idxDown++
			return slopeDown
		case '>':
			rightSlopes[pos] = idxRight
			idxRight++
			return slopeRight
		default:
			panic(r)
		}
	})

	board := Board{
		board:       b,
		downSlopes:  downSlopes,
		rightSlopes: rightSlopes,
		moves:       make(map[aoc.Location][]Destination),
	}

	fillMoves(board)

	fmt.Println(board.moves[aoc.NewLocation(4, 3, aoc.Up)])

	target := aoc.NewPosition(board.board.MaxRows-1, board.board.MaxCols-2)
	cache := make(map[Entry]struct{})

	q := []State{
		{
			Entry: Entry{
				loc: aoc.NewLocation(0, 1, aoc.Down),
			},
		},
	}

	best := 0

	for len(q) != 0 {
		s := q[0]
		q = q[1:]

		if _, exists := cache[s.Entry]; exists {
			continue
		}
		cache[s.Entry] = struct{}{}

		destinations, exists := board.moves[s.loc]
		if !exists {
			continue
		}

		for _, destination := range destinations {
			moves := s.moves + destination.moves

			if destination.loc.Pos == target {
				best = max(best, moves)
				continue
			}

			switch destination.loc.Dir {
			case aoc.Up, aoc.Down:
				down := s.down | 1<<board.downSlopes[destination.loc.Pos]
				q = append(q, State{
					Entry: Entry{
						loc:   destination.loc,
						down:  down,
						right: s.right,
					},
					moves: moves,
				})
			case aoc.Left, aoc.Right:
				right := s.right | 1<<board.rightSlopes[destination.loc.Pos]
				q = append(q, State{
					Entry: Entry{
						loc:   destination.loc,
						down:  s.down,
						right: right,
					},
					moves: moves,
				})
			}
		}
	}

	return best
}

func fillMoves(board Board) {
	waypoints := []aoc.Location{
		aoc.NewLocation(0, 1, aoc.Down),
	}

	for pos, t := range board.board.Positions {
		switch t {
		case slopeDown:
			waypoints = append(waypoints, aoc.Location{
				Pos: pos,
				Dir: aoc.Down,
			})
			waypoints = append(waypoints, aoc.Location{
				Pos: pos,
				Dir: aoc.Up,
			})
		case slopeRight:
			waypoints = append(waypoints, aoc.Location{
				Pos: pos,
				Dir: aoc.Right,
			})
			waypoints = append(waypoints, aoc.Location{
				Pos: pos,
				Dir: aoc.Left,
			})
		}
	}

	target := aoc.NewPosition(board.board.MaxRows-1, board.board.MaxCols-2)
	type entry struct {
		loc   aoc.Location
		moves int
	}

	for _, waypoint := range waypoints {
		next := waypoint.Straight(1)
		moves := 1

		q := []entry{{loc: next, moves: moves}}
		for len(q) != 0 {
			e := q[0]
			q = q[1:]

			if e.loc.Pos.Row < 0 {
				// We passed via the entrance
				continue
			}

			if exists, l2, isWaypoint := move3(board, e.loc.Straight(1), target); exists {
				if !isWaypoint {
					q = append(q, entry{loc: l2, moves: e.moves + 1})
				} else {
					board.moves[waypoint] = append(board.moves[waypoint], Destination{
						loc:   l2,
						moves: e.moves + 1,
					})
				}
			}

			if exists, l2, isWaypoint := move3(board, e.loc.Turn(aoc.Left, 1), target); exists {
				if !isWaypoint {
					q = append(q, entry{loc: l2, moves: e.moves + 1})
				} else {
					board.moves[waypoint] = append(board.moves[waypoint], Destination{
						loc:   l2,
						moves: e.moves + 1,
					})
				}
			}

			if exists, l2, isWaypoint := move3(board, e.loc.Turn(aoc.Right, 1), target); exists {
				if !isWaypoint {
					q = append(q, entry{loc: l2, moves: e.moves + 1})
				} else {
					board.moves[waypoint] = append(board.moves[waypoint], Destination{
						loc:   l2,
						moves: e.moves + 1,
					})
				}
			}
		}
	}
}

func move3(board Board, loc aoc.Location, target aoc.Position) (bool, aoc.Location, bool) {
	t := board.board.Get(loc.Pos)
	switch t {
	case forest:
		return false, aoc.Location{}, false
	case path:
		if loc.Pos == target {
			return true, loc, true
		}
		return true, loc, false
	case slopeRight, slopeDown:
		return true, loc, true
	}
	panic("unhandled case")
}

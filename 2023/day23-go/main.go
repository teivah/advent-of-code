package main

import (
	"context"
	"fmt"
	"io"
	"runtime"
	"sync"
	"time"

	aoc "github.com/teivah/advent-of-code"
	"golang.org/x/sync/errgroup"
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

type Queue struct {
	head *Node
	tail *Node
}

type Node struct {
	state State
	next  *Node
}

func (q *Queue) push(state State) {
	n := &Node{state: state}
	if q.head == nil {
		q.head = n
		q.tail = n
		return
	}
	q.tail.next = n
	q.tail = n
}

func (q *Queue) isEmpty() bool {
	return q.head == nil
}

func (q *Queue) pop() *Node {
	n := q.head

	if q.head == q.tail {
		q.head = nil
		q.tail = nil
		return n
	}

	q.head = q.head.next
	return n
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

	target := aoc.NewPosition(board.board.MaxRows-1, board.board.MaxCols-2)
	cache := make(map[Entry]struct{})

	q := Queue{}
	q.push(State{
		Entry: Entry{
			loc: aoc.NewLocation(0, 1, aoc.Down),
		},
	})

	best := 0
	conc := 100_000
	eg, _ := errgroup.WithContext(context.Background())
	eg.SetLimit(runtime.NumCPU())
	queueMu := sync.Mutex{}
	cacheMu := sync.Mutex{}
	bestMu := sync.Mutex{}
	now := time.Now()

	for {
		queueMu.Lock()
		if !q.isEmpty() {
		} else {
			queueMu.Unlock()
			_ = eg.Wait()
			queueMu.Lock()
			if q.isEmpty() {
				return best
			} else {
			}
		}

		states := make([]State, 0, conc)
		for i := 0; i < conc; i++ {
			n := q.pop()
			states = append(states, n.state)
			if q.isEmpty() {
				break
			}
		}
		queueMu.Unlock()

		eg.Go(func() error {
			localBest := 0
			var out []State
			for _, s := range states {
				if s.loc.Pos.Row < 0 || s.loc.Pos.Row >= board.board.MaxRows || s.loc.Pos.Col < 0 || s.loc.Pos.Col >= board.board.MaxCols {
					panic(s)
				}

				cacheMu.Lock()
				if _, exists := cache[s.Entry]; exists {
					cacheMu.Unlock()
					continue
				}
				cache[s.Entry] = struct{}{}
				cacheMu.Unlock()

				destinations, exists := board.moves[s.loc]
				if !exists {
					continue
				}

				for _, destination := range destinations {
					moves := s.moves + destination.moves

					if destination.loc.Pos == target {
						localBest = max(localBest, moves)
						continue
					}

					switch destination.loc.Dir {
					case aoc.Up, aoc.Down:
						down := s.down | 1<<board.downSlopes[destination.loc.Pos]
						if down != s.down {
							out = append(out, State{
								Entry: Entry{
									loc:   destination.loc,
									down:  down,
									right: s.right,
								},
								moves: moves,
							})
						}
					case aoc.Left, aoc.Right:
						right := s.right | 1<<board.rightSlopes[destination.loc.Pos]
						if right != s.right {
							out = append(out, State{
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
			}

			queueMu.Lock()
			for _, s := range out {
				q.push(s)
			}
			queueMu.Unlock()

			bestMu.Lock()
			if localBest > best {
				fmt.Println(localBest, time.Since(now))
			}
			best = max(best, localBest)
			bestMu.Unlock()

			return nil
		})
	}

	//for row := 0; row < board.board.MaxRows; row++ {
	//	for col := 0; col < board.board.MaxCols; col++ {
	//		p := aoc.NewPosition(row, col)
	//
	//		contains := false
	//		for _, dir := range []aoc.Direction{aoc.Up, aoc.Down, aoc.Left, aoc.Right} {
	//			if _, exists := bestLocations[aoc.Location{Pos: p, Dir: dir}]; exists {
	//				switch dir {
	//				case aoc.Up:
	//					fmt.Print("U")
	//				case aoc.Down:
	//					fmt.Print("D")
	//				case aoc.Left:
	//					fmt.Print("L")
	//				case aoc.Right:
	//					fmt.Print("R")
	//				}
	//				contains = true
	//				break
	//			}
	//		}
	//
	//		if contains {
	//			continue
	//		}
	//
	//		t := board.board.Get(p)
	//		switch t {
	//		case slopeDown:
	//			fmt.Print("v")
	//		case slopeRight:
	//			fmt.Print(">")
	//		case path:
	//			fmt.Print(".")
	//		case forest:
	//			fmt.Print("#")
	//		}
	//	}
	//	fmt.Println()
	//}
	//
	//fmt.Println(bestL)

	return best
}

func copyLocations(locations map[aoc.Location]struct{}) map[aoc.Location]struct{} {
	res := make(map[aoc.Location]struct{}, len(locations))
	for k, v := range locations {
		res[k] = v
	}
	return res
}

func copyLocationsS(locations []aoc.Location) []aoc.Location {
	res := make([]aoc.Location, len(locations))
	for k, v := range locations {
		res[k] = v
	}
	return res
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
		t := board.board.Get(next.Pos)
		if t == forest {
			continue
		}

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

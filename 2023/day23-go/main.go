package main

import (
	"fmt"
	"io"
	"time"

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
	loc          aoc.Location
	moves        int
	rightReduced int
	downReduced  int
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
	start := aoc.NewLocation(0, 1, aoc.Down)
	target := aoc.NewPosition(board.board.MaxRows-1, board.board.MaxCols-2)

	return dfs(board, start, target, 0, 0, 0)

	cache := make(map[Entry]struct{})

	q := Queue{}
	q.push(State{
		Entry: Entry{
			loc: start,
		},
	})

	best := 0
	now := time.Now()

	for !q.isEmpty() {
		n := q.pop()
		s := n.state

		if s.loc.Pos.Row < 0 || s.loc.Pos.Row >= board.board.MaxRows || s.loc.Pos.Col < 0 || s.loc.Pos.Col >= board.board.MaxCols {
			panic(s)
		}

		cache[s.Entry] = struct{}{}

		destinations, exists := board.moves[s.loc]
		if !exists {
			continue
		}

		for _, destination := range destinations {
			moves := s.moves + destination.moves

			if destination.loc.Pos == target {
				if moves > best {
					fmt.Println(moves, time.Since(now))
				}
				best = max(best, moves)
				continue
			}

			switch destination.loc.Dir {
			case aoc.Up, aoc.Down:
				down := s.down | 1<<board.downSlopes[destination.loc.Pos]
				// Check that we never passed this slope yet
				if down != s.down {
					// Check if we already crossed some waypoints reduced
					if down&destination.downReduced == 0 {
						down |= destination.downReduced
						entry := Entry{
							loc:   destination.loc,
							down:  down,
							right: s.right,
						}
						if _, exists := cache[entry]; !exists {
							q.push(State{
								Entry: entry,
								moves: moves,
							})
						}
					}
				}
			case aoc.Left, aoc.Right:
				right := s.right | 1<<board.rightSlopes[destination.loc.Pos]
				// Check that we never passed this slope yet
				if right != s.right {
					// Check if we already crossed some waypoints reduced
					if right&destination.rightReduced == 0 {
						right |= destination.rightReduced
						entry := Entry{
							loc:   destination.loc,
							down:  s.down,
							right: right,
						}
						if _, exists := cache[entry]; !exists {
							q.push(State{
								Entry: entry,
								moves: moves,
							})
						}
					}
				}
			}
		}
	}

	return best
}

func dfs(board Board, cur aoc.Location, target aoc.Position, rightVisited, downVisited int, moves int) int {
	if cur.Pos == target {
		return moves
	}

	destinations := board.moves[cur]
	best := -1
	for _, destination := range destinations {
		rightVisited := rightVisited
		downVisited := downVisited

		switch destination.loc.Dir {
		case aoc.Up, aoc.Down:
			v, contains := board.downSlopes[destination.loc.Pos]
			if contains {
				if (1<<v)&downVisited != 0 {
					continue
				}
				downVisited |= 1 << v
			}
		case aoc.Left, aoc.Right:
			v, contains := board.rightSlopes[destination.loc.Pos]
			if contains {
				if (1<<v)&rightVisited != 0 {
					continue
				}
				rightVisited |= 1 << v
			}
		}

		v := dfs(board, destination.loc, target, rightVisited, downVisited, moves+destination.moves)
		best = max(best, v)
	}
	return best
}

func fillMoves(board Board) {
	start := aoc.NewLocation(0, 1, aoc.Down)
	waypoints := []aoc.Location{start}

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

	//var zero aoc.Location
	//reduce(0, zero, start, board, 0, make(map[aoc.Location]bool))
	//fmt.Println(board.moves)
}

func reduce(idParent int, parent, loc aoc.Location, board Board, moves int, visited map[aoc.Location]bool) {
	if visited[loc] {
		return
	}
	visited[loc] = true

	destinations, exists := board.moves[loc]
	if !exists {
		return
	}

	if len(destinations) == 1 {
		var zero aoc.Location
		if parent == zero {
			reduce(0, loc, destinations[0].loc, board, destinations[0].moves, visited)
		} else {
			rightReduced := board.moves[parent][idParent].rightReduced
			downReduced := board.moves[parent][idParent].downReduced
			switch loc.Dir {
			case aoc.Left, aoc.Right:
				rightReduced |= 1 << board.rightSlopes[loc.Pos]
			case aoc.Up, aoc.Down:
				downReduced |= 1 << board.downSlopes[loc.Pos]
			}
			board.moves[parent][idParent] = Destination{
				loc:          destinations[0].loc,
				moves:        destinations[0].moves + moves,
				rightReduced: rightReduced,
				downReduced:  downReduced,
			}
			//delete(board.moves, loc)
			reduce(0, parent, destinations[0].loc, board, destinations[0].moves+moves, visited)
		}
		return
	}

	for id, destination := range destinations {
		reduce(id, loc, destination.loc, board, destination.moves, visited)
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

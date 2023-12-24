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

func fs2(input io.Reader) int {
	b := aoc.ParseBoard(aoc.ReaderToStrings(input), func(r rune, pos aoc.Position) trailType {
		switch r {
		case '.':
			return path
		case '#':
			return forest
		case 'v':
			return path
		case '>':
			return path
		default:
			panic(r)
		}
	})

	board := Board{
		board: b,
		moves: make(map[aoc.Location][]Destination),
	}

	g := toGraph(board)
	for k, v := range g {
		fmt.Printf("location: %v: %v, %v\n", k, len(v), v)
	}

	//for row := 0; row < board.board.MaxRows; row++ {
	//	for col := 0; col < board.board.MaxCols; col++ {
	//		if _, exists := g[aoc.NewPosition(row, col)]; exists {
	//			fmt.Print("X")
	//		} else {
	//			t := board.board.Get(aoc.NewPosition(row, col))
	//			if t == forest {
	//				fmt.Print("#")
	//			} else {
	//				fmt.Print(".")
	//			}
	//		}
	//	}
	//	fmt.Println()
	//}

	start := aoc.NewLocation(0, 1, aoc.Down)
	target := aoc.NewPosition(board.board.MaxRows-1, board.board.MaxCols-2)
	return dfs(g, start.Pos, target, make(map[aoc.Position]bool), 0)
}

var res int

func dfs(g map[aoc.Position]map[aoc.Position]int, cur aoc.Position, target aoc.Position, visited map[aoc.Position]bool, moves int) int {
	if cur == target {
		if moves > res {
			res = moves
			fmt.Println(res)
		}
		return moves
	}

	destinations := g[cur]
	best := 0
	visited[cur] = true
	for destination, distance := range destinations {
		if visited[destination] {
			continue
		}
		v := dfs(g, destination, target, visited, moves+distance)
		best = max(best, v)
	}
	delete(visited, cur)
	return best
}

func countAround(board Board, pos aoc.Position) []aoc.Direction {
	if pos.Row == 0 || pos.Row == board.board.MaxRows-1 ||
		pos.Col == 0 || pos.Col == board.board.MaxCols-1 {
		return nil
	}

	var out []aoc.Direction
	if delta := pos.Move(aoc.Up, 1); board.board.Get(delta) == path {
		out = append(out, aoc.Up)
	}
	if delta := pos.Move(aoc.Down, 1); board.board.Get(delta) == path {
		out = append(out, aoc.Down)
	}
	if delta := pos.Move(aoc.Left, 1); board.board.Get(delta) == path {
		out = append(out, aoc.Left)
	}
	if delta := pos.Move(aoc.Right, 1); board.board.Get(delta) == path {
		out = append(out, aoc.Right)
	}
	return out
}

func toGraph(board Board) map[aoc.Position]map[aoc.Position]int {
	g := make(map[aoc.Position]map[aoc.Position]int)

	target := aoc.NewPosition(board.board.MaxRows-1, board.board.MaxCols-2)
	waypoints := map[aoc.Location]bool{
		aoc.NewLocation(0, 1, aoc.Down): true,
	}

	set := make(map[aoc.Position]bool)

	for pos, t := range board.board.Positions {
		switch t {
		case path:
			around := countAround(board, pos)
			if len(around) == 3 || len(around) == 4 {
				for _, dir := range around {
					waypoints[aoc.Location{
						Pos: pos,
						Dir: dir,
					}] = true
					set[pos] = true
				}
			}
		}
	}

	set[target] = true

	for waypoint := range waypoints {
		if _, exists := g[waypoint.Pos]; !exists {
			g[waypoint.Pos] = make(map[aoc.Position]int)
		}

		next := waypoint.Straight(1)
		locations := nextWaypoint(board, set, target, next, 1, make(map[aoc.Position]bool))
		for _, l := range locations {
			g[waypoint.Pos][l.loc.Pos] = l.moves
		}
	}

	g[target] = make(map[aoc.Position]int)

	return g
}

type location struct {
	loc   aoc.Location
	moves int
}

func nextWaypoint(board Board, set map[aoc.Position]bool, target aoc.Position, cur aoc.Location, moves int, visited map[aoc.Position]bool) []location {
	if cur.Pos == target {
		return []location{{loc: cur, moves: moves}}
	}

	if cur.Pos.Row < 0 {
		return nil
	}
	t := board.board.Get(cur.Pos)
	switch t {
	case forest:
		return nil
	}
	if set[cur.Pos] {
		return []location{{loc: cur, moves: moves}}
	}

	var out []location

	p := cur.Straight(1)
	if !visited[p.Pos] {
		visited[p.Pos] = true
		out = append(out, nextWaypoint(board, set, target, p, moves+1, visited)...)
		delete(visited, p.Pos)
	}

	p = cur.Turn(aoc.Left, 1)
	if !visited[p.Pos] {
		visited[p.Pos] = true
		out = append(out, nextWaypoint(board, set, target, p, moves+1, visited)...)
		delete(visited, p.Pos)
	}

	p = cur.Turn(aoc.Right, 1)
	if !visited[p.Pos] {
		visited[p.Pos] = true
		out = append(out, nextWaypoint(board, set, target, p, moves+1, visited)...)
		delete(visited, p.Pos)
	}
	return out
}

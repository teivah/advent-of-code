package main

import (
	"io"

	aoc "github.com/teivah/advent-of-code"
)

type trailType int

const (
	path trailType = iota
	forest
	slopeUp
	slopeDown
	slopeLeft
	slopeRight
)

type Entry struct {
	loc   aoc.Location
	up    int
	down  int
	left  int
	right int
}

type State struct {
	Entry
	moves     int
	positions map[aoc.Position]struct{}
}

type Board struct {
	board       aoc.Board[trailType]
	upSlopes    map[aoc.Position]int
	downSlopes  map[aoc.Position]int
	leftSlopes  map[aoc.Position]int
	rightSlopes map[aoc.Position]int
}

func fs1(input io.Reader) int {
	var (
		idxUp       = 0
		idxDown     = 0
		idxLeft     = 0
		idxRight    = 0
		upSlopes    = make(map[aoc.Position]int)
		downSlopes  = make(map[aoc.Position]int)
		leftSlopes  = make(map[aoc.Position]int)
		rightSlopes = make(map[aoc.Position]int)
	)
	b := aoc.ParseBoard(aoc.ReaderToStrings(input), func(r rune, pos aoc.Position) trailType {
		switch r {
		case '.':
			return path
		case '#':
			return forest
		case '^':
			upSlopes[pos] = idxUp
			idxUp++
			return slopeUp
		case 'v':
			downSlopes[pos] = idxDown
			idxDown++
			return slopeDown
		case '<':
			leftSlopes[pos] = idxLeft
			idxLeft++
			return slopeLeft
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
		upSlopes:    upSlopes,
		downSlopes:  downSlopes,
		leftSlopes:  leftSlopes,
		rightSlopes: rightSlopes,
	}
	target := aoc.NewPosition(board.board.MaxRows-1, board.board.MaxCols-2)

	cache := make(map[Entry]struct{})

	q := []State{
		{
			Entry: Entry{
				loc: aoc.NewLocation(0, 1, aoc.Down),
			},
			positions: map[aoc.Position]struct{}{
				aoc.NewPosition(0, 1): {},
			},
		},
	}

	best := 0
	var bestPositions map[aoc.Position]struct{}

	for len(q) != 0 {
		s := q[0]
		q = q[1:]

		if s.loc.Pos == target {
			if s.moves > best {
				best = s.moves
				bestPositions = s.positions
			}
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

	_ = bestPositions
	//for row := 0; row < board.board.MaxRows; row++ {
	//	for col := 0; col < board.board.MaxRows; col++ {
	//		pos := aoc.NewPosition(row, col)
	//		if _, exists := bestPositions[pos]; exists {
	//			fmt.Print("O")
	//		} else {
	//			if v, exists := board.board.Positions[pos]; exists {
	//				switch v {
	//				case forest:
	//					fmt.Print("#")
	//				case path:
	//					fmt.Print(".")
	//				case slopeLeft:
	//					fmt.Print("<")
	//				case slopeRight:
	//					fmt.Print(">")
	//				case slopeUp:
	//					fmt.Print("^")
	//				case slopeDown:
	//					fmt.Print("v")
	//				}
	//			}
	//		}
	//	}
	//	fmt.Println()
	//}
	//
	return best
}

func copyPositions(positions map[aoc.Position]struct{}) map[aoc.Position]struct{} {
	res := make(map[aoc.Position]struct{}, len(positions))
	for i, p := range positions {
		res[i] = p
	}
	return res
}

func move(board Board, s State, target aoc.Location) (State, bool) {
	t := board.board.Get(target.Pos)
	switch t {
	case forest:
		return State{}, false
	case path:
		pos := copyPositions(s.positions)
		pos[target.Pos] = struct{}{}
		return State{
			Entry: Entry{
				loc: aoc.Location{
					Pos: target.Pos,
					Dir: target.Dir,
				},
				up:    s.up,
				down:  s.down,
				left:  s.left,
				right: s.right,
			},
			moves:     s.moves,
			positions: pos,
		}, true
	case slopeLeft:
		//loc := target.Move(aoc.Left, 1)
		//left := s.left | 1<<board.leftSlopes[target.Pos]
		//if left == s.left {
		//	return State{}, false
		//}
		//return State{
		//	Entry: Entry{
		//		loc: aoc.Location{
		//			Pos: loc.Pos,
		//			Dir: aoc.Left,
		//		},
		//		up:    s.up,
		//		down:  s.down,
		//		left:  left,
		//		right: s.right,
		//	},
		//	moves: s.moves + 1,
		//}, true
	case slopeRight:
		if target.Dir == aoc.Left {
			return State{}, false
		}

		loc := target.Move(aoc.Right, 1)
		right := s.right | 1<<board.rightSlopes[target.Pos]
		if right == s.right {
			return State{}, false
		}
		pos := copyPositions(s.positions)
		pos[target.Pos] = struct{}{}
		pos[loc.Pos] = struct{}{}
		return State{
			Entry: Entry{
				loc: aoc.Location{
					Pos: loc.Pos,
					Dir: aoc.Right,
				},
				up:    s.up,
				down:  s.down,
				left:  s.left,
				right: right,
			},
			moves:     s.moves + 1,
			positions: pos,
		}, true
	case slopeUp:
		//loc := target.Move(aoc.Up, 1)
		//up := s.up | 1<<board.upSlopes[target.Pos]
		//if up == s.up {
		//	return State{}, false
		//}
		//return State{
		//	Entry: Entry{
		//		loc: aoc.Location{
		//			Pos: loc.Pos,
		//			Dir: aoc.Up,
		//		},
		//		up:    up,
		//		down:  s.down,
		//		left:  s.left,
		//		right: s.right,
		//	},
		//	moves: s.moves + 1,
		//}, true
	case slopeDown:
		if target.Dir == aoc.Up {
			return State{}, false
		}

		loc := target.Move(aoc.Down, 1)
		down := s.down | 1<<board.downSlopes[target.Pos]
		if down == s.down {
			return State{}, false
		}
		pos := copyPositions(s.positions)
		pos[target.Pos] = struct{}{}
		pos[loc.Pos] = struct{}{}
		return State{
			Entry: Entry{
				loc: aoc.Location{
					Pos: loc.Pos,
					Dir: aoc.Down,
				},
				up:    s.up,
				down:  down,
				left:  s.left,
				right: s.right,
			},
			moves:     s.moves + 1,
			positions: pos,
		}, true
	}
	panic("unhandled case")
}

func fs2(input io.Reader) int {
	var (
		idxUp       = 0
		idxDown     = 0
		idxLeft     = 0
		idxRight    = 0
		upSlopes    = make(map[aoc.Position]int)
		downSlopes  = make(map[aoc.Position]int)
		leftSlopes  = make(map[aoc.Position]int)
		rightSlopes = make(map[aoc.Position]int)
	)
	b := aoc.ParseBoard(aoc.ReaderToStrings(input), func(r rune, pos aoc.Position) trailType {
		switch r {
		case '.':
			return path
		case '#':
			return forest
		case '^':
			upSlopes[pos] = idxUp
			idxUp++
			return slopeUp
		case 'v':
			downSlopes[pos] = idxDown
			idxDown++
			return slopeDown
		case '<':
			leftSlopes[pos] = idxLeft
			idxLeft++
			return slopeLeft
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
		upSlopes:    upSlopes,
		downSlopes:  downSlopes,
		leftSlopes:  leftSlopes,
		rightSlopes: rightSlopes,
	}
	target := aoc.NewPosition(board.board.MaxRows-1, board.board.MaxCols-2)

	cache := make(map[Entry]struct{})

	q := []State{
		{
			Entry: Entry{
				loc: aoc.NewLocation(0, 1, aoc.Down),
			},
			positions: map[aoc.Position]struct{}{
				aoc.NewPosition(0, 1): {},
			},
		},
	}

	best := 0
	var bestPositions map[aoc.Position]struct{}

	for len(q) != 0 {
		s := q[0]
		q = q[1:]

		if s.loc.Pos == target {
			if s.moves > best {
				best = s.moves
				bestPositions = s.positions
			}
			continue
		}

		if _, exists := cache[s.Entry]; exists {
			continue
		}
		cache[s.Entry] = struct{}{}

		s.moves++
		if e2, exists := move2(board, s, s.loc.Straight(1)); exists {
			q = append(q, e2)
		}
		if e2, exists := move2(board, s, s.loc.Turn(aoc.Left, 1)); exists {
			q = append(q, e2)
		}
		if e2, exists := move2(board, s, s.loc.Turn(aoc.Right, 1)); exists {
			q = append(q, e2)
		}
	}

	_ = bestPositions
	//for row := 0; row < board.board.MaxRows; row++ {
	//	for col := 0; col < board.board.MaxRows; col++ {
	//		pos := aoc.NewPosition(row, col)
	//		if _, exists := bestPositions[pos]; exists {
	//			fmt.Print("O")
	//		} else {
	//			if v, exists := board.board.Positions[pos]; exists {
	//				switch v {
	//				case forest:
	//					fmt.Print("#")
	//				case path:
	//					fmt.Print(".")
	//				case slopeLeft:
	//					fmt.Print("<")
	//				case slopeRight:
	//					fmt.Print(">")
	//				case slopeUp:
	//					fmt.Print("^")
	//				case slopeDown:
	//					fmt.Print("v")
	//				}
	//			}
	//		}
	//	}
	//	fmt.Println()
	//}
	//
	return best
}

func move2(board Board, s State, target aoc.Location) (State, bool) {
	t := board.board.Get(target.Pos)
	switch t {
	case forest:
		return State{}, false
	case path:
		pos := copyPositions(s.positions)
		pos[target.Pos] = struct{}{}
		return State{
			Entry: Entry{
				loc: aoc.Location{
					Pos: target.Pos,
					Dir: target.Dir,
				},
				up:    s.up,
				down:  s.down,
				left:  s.left,
				right: s.right,
			},
			moves:     s.moves,
			positions: pos,
		}, true
	case slopeLeft:
		//loc := target.Move(aoc.Left, 1)
		//left := s.left | 1<<board.leftSlopes[target.Pos]
		//if left == s.left {
		//	return State{}, false
		//}
		//return State{
		//	Entry: Entry{
		//		loc: aoc.Location{
		//			Pos: loc.Pos,
		//			Dir: aoc.Left,
		//		},
		//		up:    s.up,
		//		down:  s.down,
		//		left:  left,
		//		right: s.right,
		//	},
		//	moves: s.moves + 1,
		//}, true
	case slopeRight:
		loc := target.Straight(1)
		right := s.right | 1<<board.rightSlopes[target.Pos]
		if right == s.right {
			return State{}, false
		}
		pos := copyPositions(s.positions)
		pos[target.Pos] = struct{}{}
		pos[loc.Pos] = struct{}{}
		return State{
			Entry: Entry{
				loc: aoc.Location{
					Pos: loc.Pos,
					Dir: loc.Dir,
				},
				up:    s.up,
				down:  s.down,
				left:  s.left,
				right: right,
			},
			moves:     s.moves + 1,
			positions: pos,
		}, true
	case slopeUp:
		//loc := target.Move(aoc.Up, 1)
		//up := s.up | 1<<board.upSlopes[target.Pos]
		//if up == s.up {
		//	return State{}, false
		//}
		//return State{
		//	Entry: Entry{
		//		loc: aoc.Location{
		//			Pos: loc.Pos,
		//			Dir: aoc.Up,
		//		},
		//		up:    up,
		//		down:  s.down,
		//		left:  s.left,
		//		right: s.right,
		//	},
		//	moves: s.moves + 1,
		//}, true
	case slopeDown:
		loc := target.Straight(1)
		down := s.down | 1<<board.downSlopes[target.Pos]
		if down == s.down {
			return State{}, false
		}
		pos := copyPositions(s.positions)
		pos[target.Pos] = struct{}{}
		pos[loc.Pos] = struct{}{}
		return State{
			Entry: Entry{
				loc: aoc.Location{
					Pos: loc.Pos,
					Dir: loc.Dir,
				},
				up:    s.up,
				down:  down,
				left:  s.left,
				right: s.right,
			},
			moves:     s.moves + 1,
			positions: pos,
		}, true
	}
	panic("unhandled case")
}

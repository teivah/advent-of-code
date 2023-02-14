package main

import (
	"fmt"
	"io"
	"strings"

	aoc "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	lines := aoc.ReaderToStrings(input)
	board := toBoard(lines)
	return board.best()
}

/*
- never stop on space outside room
- can't move into a room unless it's their destination and remaining pod in this room is the correct one
- stop in hallway: stay in that spot until it can move into a room
*/
type State struct {
	board  *Board
	energy int
}

func newState(board *Board, energy int, from, to aoc.Position) State {
	pods := make(map[aoc.Position]Pod, len(board.pods))
	for k, pod := range board.pods {
		if k == from {
			pod.pos = to
			pods[to] = pod
		} else {
			pods[k] = pod
		}
	}

	return State{
		board: &Board{
			grid: board.grid,
			pods: pods,
		},
		energy: energy,
	}
}

func (s State) over() bool {
	sum := 0
	for pos, pod := range s.board.pods {
		if pos.Col != pod.targetCol {
			return false
		}
		sum++
	}
	return true
}

func key(pods map[aoc.Position]Pod) string {
	s := make([]Pod, len(pods))
	for _, v := range pods {
		s[v.id] = v
	}

	sb := strings.Builder{}
	for _, pod := range s {
		sb.WriteString(fmt.Sprintf("%d:%v;", pod.id, pod.pos))
	}
	return sb.String()
}

/*


#############
#...B.....A.#
###B#.#C#.###
  #A#D#C#D#
  #########

row=1, col=8,row=3, col=9 5443
row=3, col=5,row=2, col=9 5443 <-- issue
row=1, col=4,row=3, col=5 5473
row=2, col=3,row=2, col=5 5513
row=1, col=10,row=2, col=3 5521

*/
func (b *Board) best() int {
	var q []State
	found := false
	best := aoc.NewMiner()
	visited := make(map[string]int)
	q = append(q, State{
		board:  b,
		energy: 0,
	})

	for len(q) != 0 {
		s := q[0]
		q = q[1:]

		if found && s.energy >= best.Get() {
			continue
		}

		k := key(s.board.pods)
		if v, contains := visited[k]; contains {
			if v <= s.energy {
				continue
			}
		}
		visited[k] = s.energy

		//fmt.Println(k, s.energy)
		//fmt.Println(s.board)

		if s.over() {
			fmt.Println(s.energy)
			found = true
			best.Add(s.energy)
			continue
		}

		for _, pod := range s.board.pods {
			if pod.isInTarget(s.board) {
				continue
			}
			options := pod.bfs(s.board)
			if len(options) == 0 {
				continue
			}

			for _, destination := range options {
				if pod.pos.Row == 1 && destination.Row == 1 {
					continue
				}

				moves := 0
				if pod.pos.Row == 1 && destination.Row == 1 {
					moves = aoc.Abs(destination.Col - pod.pos.Col)
				} else {
					moves = aoc.Abs(destination.Col-pod.pos.Col) + aoc.Abs(1-pod.pos.Row) + aoc.Abs(1-destination.Row)
				}

				s2 := newState(s.board, s.energy+moves*pod.energy, pod.pos, destination)
				q = append(q, s2)
			}
		}
	}
	return best.Get()
}

type Board struct {
	grid map[aoc.Position]Unit
	pods map[aoc.Position]Pod
}

func (b *Board) String() string {
	s := ""

	for row := 0; row < 7; row++ {
		for col := 0; col < 13; col++ {
			pos := aoc.Position{row, col}
			if v, exists := b.pods[pos]; exists {
				s += v.name
				continue
			}
			v, exists := b.grid[pos]
			if !exists {
				s += " "
			} else {
				if v == wall {
					s += "#"
				} else {
					s += "."
				}
			}
		}
		s += "\n"
	}

	return s
}

func (b *Board) isPositionAllowedAndFree(pos aoc.Position) bool {
	v, exists := b.grid[pos]
	if !exists {
		return false
	}
	if v == wall {
		return false
	}
	_, exists = b.pods[pos]
	if !exists {
		return true
	}
	return false
}

func (b *Board) isPositionAllowed(pos aoc.Position) bool {
	v, exists := b.grid[pos]
	if !exists {
		return false
	}
	return v == empty
}

func toBoard(lines []string) *Board {
	grid := make(map[aoc.Position]Unit)
	pods := make(map[aoc.Position]Pod)
	id := 0
	for row := 0; row < len(lines); row++ {
		for col := 0; col < len(lines[row]); col++ {
			r := lines[row][col]
			pos := aoc.Position{row, col}
			switch r {
			case '#':
				grid[pos] = wall
			case '.':
				grid[pos] = empty
			case ' ':
			default:
				grid[pos] = empty
				targetCol := 0
				energy := 0
				if r == 'A' {
					targetCol = 3
					energy = 1
				} else if r == 'B' {
					targetCol = 5
					energy = 10
				} else if r == 'C' {
					targetCol = 7
					energy = 100
				} else if r == 'D' {
					targetCol = 9
					energy = 1000
				}
				pods[pos] = Pod{
					id:        id,
					energy:    energy,
					pos:       pos,
					targetCol: targetCol,
					name:      string(r),
				}
				id++
			}
		}
	}
	return &Board{
		grid: grid,
		pods: pods,
	}
}

type Pod struct {
	id        int
	pos       aoc.Position
	targetCol int
	name      string
	energy    int
}

func (p Pod) isInHallway() bool {
	return p.pos.Row == 1
}

func (p Pod) isInTarget(board *Board) bool {
	if p.targetCol != p.pos.Col {
		return false
	}
	if p.pos.Row == 1 {
		return false
	}

	pos := p.pos
	for {
		pos = pos.Delta(1, 0)
		if !board.isPositionAllowed(pos) {
			break
		}
		if v, exists := board.pods[pos]; exists {
			if p.name != v.name {
				return false
			}
		}

	}
	return true
}

func (p Pod) bfs(board *Board) []aoc.Position {
	if p.isInTarget(board) {
		return nil
	}

	var q []aoc.Position
	visited := make(map[aoc.Position]bool)
	if p.pos.Row == 1 {
		q = append(q, p.pos.Delta(0, -1))
		q = append(q, p.pos.Delta(0, 1))
		visited[p.pos.Delta(1, 0)] = true
	} else if p.pos.Row == 2 {
		q = append(q, p.pos.Delta(-1, 0))
		visited[p.pos] = true
	} else {
		if !board.isPositionAllowedAndFree(p.pos.Delta(-1, 0)) {
			return nil
		}
		q = append(q, p.pos.Delta(-2, 0))
		visited[p.pos.Delta(-1, 0)] = true
	}
	var res []aoc.Position
	for len(q) != 0 {
		pos := q[0]
		q = q[1:]
		if !board.isPositionAllowedAndFree(pos) {
			continue
		}
		if visited[pos] {
			continue
		}
		visited[pos] = true

		if p.pos.Row == 1 {
			if pos.Col == p.targetCol {
				if pos.Row == 1 {
					p2 := pos.Delta(0, -1)
					if !visited[p2] {
						q = append(q, p2)
					}
					p2 = pos.Delta(0, 1)
					if !visited[p2] {
						q = append(q, p2)
					}
					p2 = pos.Delta(1, 0)
					if !visited[p2] {
						q = append(q, p2)
					}
				} else if pos.Row == 2 {
					if v, exists := board.pods[aoc.Position{3, p.targetCol}]; exists {
						if p.name == v.name {
							return []aoc.Position{{2, p.targetCol}}
						}
						return nil
					}
					return []aoc.Position{{3, p.targetCol}}
				} else {
					return []aoc.Position{{3, p.targetCol}}
				}
			} else {
				p2 := pos.Delta(0, -1)
				if !visited[p2] {
					q = append(q, p2)
				}
				p2 = pos.Delta(0, 1)
				if !visited[p2] {
					q = append(q, p2)
				}
				p2 = pos.Delta(1, 0)
				if !visited[p2] {
					q = append(q, p2)
				}
			}
		} else {
			if pos.Col == p.targetCol {
				if pos.Row == 2 {
					if v, exists := board.pods[aoc.Position{3, p.targetCol}]; exists {
						if p.name == v.name {
							res = append(res, aoc.Position{2, p.targetCol})
						}
						continue
					}
					res = append(res, aoc.Position{3, p.targetCol})
					continue
				} else if pos.Row == 3 {
					res = append(res, pos)
					continue
				} else {
					p2 := pos.Delta(0, -1)
					if !visited[p2] {
						q = append(q, p2)
					}
					p2 = pos.Delta(0, 1)
					if !visited[p2] {
						q = append(q, p2)
					}
					p2 = pos.Delta(1, 0)
					if !visited[p2] {
						q = append(q, p2)
					}
				}
			} else {
				if pos.Col != 3 && pos.Col != 5 && pos.Col != 7 && pos.Col != 9 {
					res = append(res, pos)
				}
				p2 := pos.Delta(0, -1)
				if !visited[p2] {
					q = append(q, p2)
				}
				p2 = pos.Delta(0, 1)
				if !visited[p2] {
					q = append(q, p2)
				}
				p2 = pos.Delta(1, 0)
				if !visited[p2] {
					q = append(q, p2)
				}
			}
		}
	}
	return res
}

type Unit rune

const (
	wall  = '#'
	empty = '.'
)

func fs2(input io.Reader) int {
	lines := aoc.ReaderToStrings(input)
	board := toBoard(lines)
	return board.best()
}

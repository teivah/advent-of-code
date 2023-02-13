package main

import (
	"bufio"
	"fmt"
	"io"

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

func (s State) over() bool {
	for pos, pod := range s.board.pods {
		if pos.Col != pod.targetCol {
			return false
		}
	}
	return true
}

func (s State) copy() State {
	pods := make(map[aoc.Position]Pod, len(s.board.pods))
	for k, v := range s.board.pods {
		pods[k] = v
	}
	board := &Board{
		grid: s.board.grid,
		pods: pods,
	}
	return State{
		board:  board,
		energy: s.energy,
	}
}

func key(pods map[aoc.Position]Pod) string {
	return fmt.Sprintf("%v", pods)
}

func (b *Board) best() int {
	var q []State
	best := aoc.NewMaxer()
	visited := make(map[string]int)
	q = append(q, State{
		board:  b,
		energy: 0,
	})

	for len(q) != 0 {
		s := q[0]
		q = q[1:]

		fmt.Println(len(q))

		k := key(s.board.pods)
		if v, contains := visited[k]; contains {
			if v < s.energy {
				continue
			}
		}
		visited[k] = s.energy

		if s.over() {
			best.Add(s.energy)
			continue
		}

		for _, pod := range s.board.pods {
			if pod.target {
				continue
			}
			options := pod.options(s.board)
			if len(options) == 0 {
				continue
			}

			oldDestination := pod.pos
			for _, destination := range options {
				pod.move(s.board, destination)
				q = append(q, State{
					board:  s.board,
					energy: s.energy + oldDestination.Manhattan(destination)*pod.energy,
				})
				pod.move(s.board, oldDestination)
			}
		}
	}
	return best.Get()
}

type Board struct {
	grid map[aoc.Position]Unit
	pods map[aoc.Position]Pod
}

func (b *Board) isEmpty(pos aoc.Position) bool {
	v, exists := b.grid[pos]
	if !exists {
		return false
	}
	if v == wall {
		return false
	}
	_, exists = b.pods[pos]
	if exists {
		return false
	}
	return true
}

func (p *Pod) move(board *Board, pos aoc.Position) {
	delete(board.pods, pos)
	p.pos = pos
	board.pods[pos] = *p

	p.target = p.pos.Col == p.targetCol
}

func toBoard(lines []string) *Board {
	grid := make(map[aoc.Position]Unit)
	pods := make(map[aoc.Position]Pod)
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
					energy:    energy,
					pos:       pos,
					targetCol: targetCol,
					name:      string(r),
				}
			}
		}
	}
	return &Board{
		grid: grid,
		pods: pods,
	}
}

type Pod struct {
	pos       aoc.Position
	targetCol int
	target    bool
	name      string
	energy    int
}

func (p Pod) isInHallway() bool {
	return p.pos.Row == 1
}

func (p Pod) isInTarget() bool {
	return p.targetCol == p.pos.Col
}

func (p Pod) options(board *Board) []aoc.Position {
	if p.isInTarget() {
		return nil
	}

	if p.isInHallway() {
		pos := p.pos
		if p.pos.Col < p.targetCol {
			// Right
			for pos.Col != p.targetCol {
				pos = pos.Delta(0, 1)
			}
		} else {
			// Left
			for pos.Col != p.targetCol {
				pos = pos.Delta(0, -1)
			}
		}
		if v, contains := board.pods[pos.Delta(2, 0)]; !contains {
			if v.name != p.name {
				return nil
			}
			pos = pos.Delta(2, 0)
		} else {
			pos = pos.Delta(1, 0)
		}
		return []aoc.Position{pos}
	} else {
		pos := p.pos.Delta(-1, 0)
		if !board.isEmpty(pos) {
			return nil
		}
		if pos.Row != 1 {
			pos = p.pos.Delta(-1, 0)
			if !board.isEmpty(pos) {
				return nil
			}
		}

		posLeft := pos.Delta(0, -1)
		posRight := pos.Delta(0, 1)
		return append(p.findHallwayOptions(board, posLeft, aoc.Left),
			p.findHallwayOptions(board, posRight, aoc.Right)...)
	}
}

func (p Pod) findHallwayOptions(board *Board, pos aoc.Position, direction aoc.Direction) []aoc.Position {
	v, exists := board.grid[pos]
	if !exists {
		return nil
	}
	if v == wall {
		return nil
	}

	_, contains := board.pods[pos]
	if contains {
		return nil
	}

	return append(p.findHallwayOptions(board, pos.Move(direction, 1), direction), pos)
}

type Unit rune

const (
	wall  = '#'
	empty = '.'
)

func fs2(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
		line := scanner.Text()
		_ = line
	}

	return 42
}

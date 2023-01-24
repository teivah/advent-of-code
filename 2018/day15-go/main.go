package main

import (
	"fmt"
	"io"
	"math"

	lib "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	lines := lib.ReaderToStrings(input)

	grid := make([][]Cell, len(lines))
	var goblins []*Unit
	var elves []*Unit
	for row, line := range lines {
		grid[row] = make([]Cell, 0, len(line))
		for col := 0; col < len(line); col++ {
			switch line[col] {
			case '#':
				grid[row] = append(grid[row], Cell{cellType: wall})
			case '.':
				grid[row] = append(grid[row], Cell{cellType: empty})
			case 'G':
				g := &Unit{unitType: goblin, power: 3, hitPoints: 200, position: Position{row, col}}
				goblins = append(goblins, g)
				grid[row] = append(grid[row], Cell{cellType: empty, unit: g})
			case 'E':
				e := &Unit{unitType: elf, power: 3, hitPoints: 200, position: Position{row, col}}
				elves = append(elves, e)
				grid[row] = append(grid[row], Cell{cellType: empty, unit: e})
			}
		}
	}

	board := &Board{
		grid:    grid,
		goblins: goblins,
		elves:   elves,
	}

	rounds := 0
	for {
		fmt.Println(rounds)
		if len(board.goblins) == 0 || len(board.elves) == 0 {
			break
		}

		board.round()
		board.print()
		rounds++
	}

	sum := 0
	if len(board.goblins) != 0 {
		for _, goblin := range board.goblins {
			sum += goblin.hitPoints
		}
	} else {
		for _, elf := range board.elves {
			sum += elf.hitPoints
		}
	}

	return (rounds - 1) * sum
}

type Board struct {
	grid    [][]Cell
	goblins []*Unit
	elves   []*Unit
}

func (b *Board) getUnit(pos Position) *Unit {
	return b.grid[pos.row][pos.col].unit
}

func (b *Board) isEmpty(pos Position) bool {
	cell := b.grid[pos.row][pos.col]
	if cell.unit != nil {
		return false
	}
	return cell.cellType == empty
}

func (b *Board) round() {
	for _, elf := range b.elves {
		elf.turnOver = false
	}
	for _, goblin := range b.goblins {
		goblin.turnOver = false
	}

	for row := 0; row < len(b.grid); row++ {
		for col := 0; col < len(b.grid[0]); col++ {
			cell := b.grid[row][col]
			if cell.unit == nil {
				continue
			}

			cell.unit.turn(b)
		}
	}
}

func (b *Board) updateIsInRange() {
	elvesInRange := make(map[*Unit]struct{})
	goblinsInRange := make(map[*Unit]struct{})

	for elf := range b.elves {
		for goblin := range b.goblins {
			if distance(b.elves[elf].position, b.goblins[goblin].position) == 1 {
				elvesInRange[b.elves[elf]] = struct{}{}
				goblinsInRange[b.goblins[goblin]] = struct{}{}

				b.elves[elf].isInRange = true
				b.goblins[goblin].isInRange = true
			}
		}
	}

	for _, goblin := range b.goblins {
		_, exists := goblinsInRange[goblin]
		if !exists {
			goblin.isInRange = false
		}
	}

	for _, elf := range b.elves {
		_, exists := elvesInRange[elf]
		if !exists {
			elf.isInRange = false
		}
	}
}

func distance(from, to Position) int {
	return lib.Abs(from.row-to.row) + lib.Abs(from.col-to.col)
}

func (b *Board) print() {
	for row := 0; row < len(b.grid); row++ {
		for col := 0; col < len(b.grid[0]); col++ {
			cell := b.grid[row][col]
			if cell.unit != nil {
				if cell.unit.unitType == goblin {
					fmt.Print("G")
				} else {
					fmt.Print("E")
				}
			} else {
				if cell.cellType == wall {
					fmt.Print("#")
				} else {
					fmt.Print(".")
				}
			}
		}
		fmt.Println()
	}
	fmt.Println()
}

type Cell struct {
	cellType CellType
	unit     *Unit
}

type CellType int

const (
	wall CellType = iota
	empty
)

type Unit struct {
	unitType  UnitType
	power     int
	hitPoints int
	isInRange bool
	position  Position
	turnOver  bool
}

func (u *Unit) turn(board *Board) {
	if u.turnOver {
		return
	}

	defer board.updateIsInRange()

	u.turnOver = true

	if !u.isInRange {
		// Check moves
		var targets []*Unit
		if u.unitType == goblin {
			targets = board.elves
		} else {
			targets = board.goblins
		}

		options := getPossibleSquares(board, targets)
		if len(options) == 0 {
			return
		}

		// Move
		to := shortest(board, u.position, options)
		if to == nil {
			return
		}

		oldRow := u.position.row
		oldCol := u.position.col

		u.position.row = to.row
		u.position.col = to.col

		board.grid[to.row][to.col].unit = board.grid[oldRow][oldCol].unit
		board.grid[oldRow][oldCol].unit = nil
		board.updateIsInRange()
	}

	// Attack
	target := u.selectTarget(board)
	if target == nil {
		return
	}
	t := board.getUnit(*target)
	t.hitPoints -= u.power
	if t.hitPoints <= 0 {
		board.grid[target.row][target.col].unit = nil

		if t.unitType == elf {
			if len(board.elves) == 1 {
				board.elves = nil
			} else {
				i := 0
				for ; i < len(board.elves); i++ {
					elf := board.elves[i]
					if elf.position == t.position {
						break
					}
				}

				if i == 0 {
					board.elves = board.elves[1:]
				} else if i == len(board.elves)-1 {
					board.elves = board.elves[:len(board.elves)-1]
				} else {
					res := make([]*Unit, 0, len(board.elves)-1)
					for j := 0; j < i; j++ {
						res = append(res, board.elves[j])
					}
					for j := i + 1; j < len(board.elves); j++ {
						res = append(res, board.elves[j])
					}
					board.elves = res
				}
			}
		} else {
			if len(board.goblins) == 1 {
				board.goblins = nil
			} else {
				i := 0
				for ; i < len(board.goblins); i++ {
					goblin := board.goblins[i]
					if goblin.position == t.position {
						break
					}
				}

				if i == 0 {
					board.goblins = board.goblins[1:]
				} else if i == len(board.goblins)-1 {
					board.goblins = board.goblins[:len(board.goblins)-1]
				} else {
					res := make([]*Unit, 0, len(board.goblins)-1)
					for j := 0; j < i; j++ {
						res = append(res, board.goblins[j])
					}
					for j := i + 1; j < len(board.goblins); j++ {
						res = append(res, board.goblins[j])
					}
					board.goblins = res
				}
			}
		}
	}
}

func (u *Unit) selectTarget(board *Board) *Position {
	var target *Position
	minHitPoints := math.MaxInt

	top := u.position.delta(-1, 0)
	cell := board.grid[top.row][top.col]
	if cell.unit != nil && cell.unit.unitType != u.unitType {
		target = &top
		minHitPoints = cell.unit.hitPoints
	}

	down := u.position.delta(1, 0)
	cell = board.grid[down.row][down.col]
	if cell.unit != nil && cell.unit.unitType != u.unitType {
		if cell.unit.hitPoints < minHitPoints {
			target = &down
			minHitPoints = cell.unit.hitPoints
		} else if cell.unit.hitPoints == minHitPoints {
			x := *target
			y := x.min(down)
			target = &y
		}
	}

	left := u.position.delta(0, -1)
	cell = board.grid[left.row][left.col]
	if cell.unit != nil && cell.unit.unitType != u.unitType {
		if cell.unit.hitPoints < minHitPoints {
			target = &left
			minHitPoints = cell.unit.hitPoints
		} else if cell.unit.hitPoints == minHitPoints {
			x := *target
			y := x.min(left)
			target = &y
		}
	}

	right := u.position.delta(0, 1)
	cell = board.grid[right.row][right.col]
	if cell.unit != nil && cell.unit.unitType != u.unitType {
		if cell.unit.hitPoints < minHitPoints {
			target = &right
			minHitPoints = cell.unit.hitPoints
		} else if cell.unit.hitPoints == minHitPoints {
			x := *target
			y := x.min(right)
			target = &y
		}
	}

	return target
}

func shortest(board *Board, from Position, options []Position) *Position {
	tos := make(map[Position]struct{}, len(options))
	for _, option := range options {
		tos[option] = struct{}{}
	}

	i, position := bfs2(board, from, tos)
	if i == math.MaxInt {
		return nil
	}
	return &position
}

type State struct {
	pos       Position
	current   int
	firstStep *Position
}

func bfs(board *Board, from Position, to Position) (int, Position) {
	q := []State{{pos: from, current: 0}}
	visited := make(map[Position]int)

	for len(q) != 0 {
		levels := len(q)

		min := 0
		var step *Position

		for i := 0; i < levels; i++ {
			state := q[0]
			q = q[1:]

			if state.pos == to {
				min = state.current
				if step == nil {
					step = state.firstStep
				} else {
					x := *step
					y := x.min(*state.firstStep)
					step = &y
				}
			}

			if state.pos != from && !board.isEmpty(state.pos) {
				continue
			}

			if v, exists := visited[state.pos]; exists {
				if state.current > v {
					continue
				}
			}
			visited[state.pos] = state.current

			state.current++

			up := state.pos.delta(-1, 0)
			if state.firstStep == nil {
				q = append(q, State{
					pos:       up,
					current:   state.current,
					firstStep: &up,
				})
			} else {
				q = append(q, State{
					pos:       up,
					current:   state.current,
					firstStep: state.firstStep,
				})
			}

			down := state.pos.delta(1, 0)
			if state.firstStep == nil {
				q = append(q, State{
					pos:       down,
					current:   state.current,
					firstStep: &down,
				})
			} else {
				q = append(q, State{
					pos:       down,
					current:   state.current,
					firstStep: state.firstStep,
				})
			}

			left := state.pos.delta(0, -1)
			if state.firstStep == nil {
				q = append(q, State{
					pos:       left,
					current:   state.current,
					firstStep: &left,
				})
			} else {
				q = append(q, State{
					pos:       left,
					current:   state.current,
					firstStep: state.firstStep,
				})
			}

			right := state.pos.delta(0, 1)
			if state.firstStep == nil {
				q = append(q, State{
					pos:       right,
					current:   state.current,
					firstStep: &right,
				})
			} else {
				q = append(q, State{
					pos:       right,
					current:   state.current,
					firstStep: state.firstStep,
				})
			}
		}

		if step != nil {
			return min, *step
		}
	}

	return math.MaxInt, Position{}
}

func bfs2(board *Board, from Position, tos map[Position]struct{}) (int, Position) {
	q := []State{{pos: from, current: 0}}
	visited := make(map[Position]int)

	for len(q) != 0 {
		levels := len(q)

		min := 0
		var step *Position

		for i := 0; i < levels; i++ {
			state := q[0]
			q = q[1:]

			if _, exists := tos[state.pos]; exists {
				min = state.current
				if step == nil {
					step = state.firstStep
				} else {
					x := *step
					y := x.min(*state.firstStep)
					step = &y
				}
			}

			if state.pos != from && !board.isEmpty(state.pos) {
				continue
			}

			if v, exists := visited[state.pos]; exists {
				if state.current > v {
					continue
				}
			}
			visited[state.pos] = state.current

			state.current++

			up := state.pos.delta(-1, 0)
			if state.firstStep == nil {
				q = append(q, State{
					pos:       up,
					current:   state.current,
					firstStep: &up,
				})
			} else {
				q = append(q, State{
					pos:       up,
					current:   state.current,
					firstStep: state.firstStep,
				})
			}

			down := state.pos.delta(1, 0)
			if state.firstStep == nil {
				q = append(q, State{
					pos:       down,
					current:   state.current,
					firstStep: &down,
				})
			} else {
				q = append(q, State{
					pos:       down,
					current:   state.current,
					firstStep: state.firstStep,
				})
			}

			left := state.pos.delta(0, -1)
			if state.firstStep == nil {
				q = append(q, State{
					pos:       left,
					current:   state.current,
					firstStep: &left,
				})
			} else {
				q = append(q, State{
					pos:       left,
					current:   state.current,
					firstStep: state.firstStep,
				})
			}

			right := state.pos.delta(0, 1)
			if state.firstStep == nil {
				q = append(q, State{
					pos:       right,
					current:   state.current,
					firstStep: &right,
				})
			} else {
				q = append(q, State{
					pos:       right,
					current:   state.current,
					firstStep: state.firstStep,
				})
			}
		}

		if step != nil {
			return min, *step
		}
	}

	return math.MaxInt, Position{}
}

func getPossibleSquares(board *Board, targets []*Unit) []Position {
	var positions []Position
	for _, target := range targets {
		pos := target.position

		up := pos.delta(-1, 0)
		if board.isEmpty(up) {
			positions = append(positions, up)
		}

		down := pos.delta(1, 0)
		if board.isEmpty(down) {
			positions = append(positions, down)
		}

		left := pos.delta(0, -1)
		if board.isEmpty(left) {
			positions = append(positions, left)
		}

		right := pos.delta(0, 1)
		if board.isEmpty(right) {
			positions = append(positions, right)
		}
	}
	return positions
}

type Position struct {
	row int
	col int
}

func (p Position) min(p2 Position) Position {
	if p.row < p2.row {
		return p
	}
	if p2.row < p.row {
		return p2
	}
	if p.col <= p2.col {
		return p
	}
	return p2
}

func (p Position) delta(row, col int) Position {
	p.row += row
	p.col += col
	return p
}

type UnitType int

const (
	goblin UnitType = iota
	elf
)

func fs2(input io.Reader) int {
	lines := lib.ReaderToStrings(input)

	for power := 4; ; power++ {
		fmt.Println("power:", power)
		grid := make([][]Cell, len(lines))
		var goblins []*Unit
		var elves []*Unit
		for row, line := range lines {
			grid[row] = make([]Cell, 0, len(line))
			for col := 0; col < len(line); col++ {
				switch line[col] {
				case '#':
					grid[row] = append(grid[row], Cell{cellType: wall})
				case '.':
					grid[row] = append(grid[row], Cell{cellType: empty})
				case 'G':
					g := &Unit{unitType: goblin, power: 3, hitPoints: 200, position: Position{row, col}}
					goblins = append(goblins, g)
					grid[row] = append(grid[row], Cell{cellType: empty, unit: g})
				case 'E':
					e := &Unit{unitType: elf, power: power, hitPoints: 200, position: Position{row, col}}
					elves = append(elves, e)
					grid[row] = append(grid[row], Cell{cellType: empty, unit: e})
				}
			}
		}

		board := &Board{
			grid:    grid,
			goblins: goblins,
			elves:   elves,
		}

		numberElves := len(board.elves)
		rounds := 0
		for {
			fmt.Println(rounds)
			if len(board.goblins) == 0 || len(board.elves) != numberElves {
				break
			}

			board.round()
			board.print()
			rounds++
		}

		if len(board.elves) != numberElves {
			continue
		}
		sum := 0
		if len(board.goblins) != 0 {
			for _, goblin := range board.goblins {
				sum += goblin.hitPoints
			}
		} else {
			for _, elf := range board.elves {
				sum += elf.hitPoints
			}
		}

		return (rounds - 1) * sum
	}
}

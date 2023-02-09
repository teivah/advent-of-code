package main

import (
	aoc "github.com/teivah/advent-of-code"
)

func fs1(from, to position) int {
	b := Board{from, to}
	return b.best()
}

func (b Board) best() int {
	const (
		maxRow = 100
		maxCol = 100
	)

	best := aoc.NewMaxer()
	for row := 0; row <= maxRow; row++ {
		for col := 0; col <= maxCol; col++ {
			h, inside := b.fire(position{row, col})
			if inside {
				best.Add(h)
			}
		}
	}

	return best.Get()
}

func (b Board) fire(velocity position) (int, bool) {
	var current position
	var previous position
	maxHeight := aoc.NewMaxer()
	for {
		current.col += velocity.col
		current.row += velocity.row
		velocity.row--
		if velocity.col > 0 {
			velocity.col--
		} else if velocity.col < 0 {
			velocity.col++
		}
		maxHeight.Add(current.row)

		if b.isInsideTarget(current) {
			return maxHeight.Get(), true
		}

		if b.isOver(current, current.row < previous.row) {
			return 0, false
		}
		previous = current
	}
}

type Board struct {
	from position
	to   position
}

func (b Board) isInsideTarget(pos position) bool {
	return pos.row >= b.from.row && pos.row <= b.to.row &&
		pos.col >= b.from.col && pos.col <= b.to.col
}

func (b Board) isOver(pos position, down bool) bool {
	if !down {
		return false
	}

	return pos.row < b.from.row
}

type position struct {
	row int
	col int
}

func fs2(from, to position) int {
	b := Board{from, to}
	return b.all()
}

func (b Board) all() int {
	const (
		minRow = -100
		minCol = -100
		maxRow = 400
		maxCol = 400
	)

	sum := 0
	for row := minRow; row <= maxRow; row++ {
		for col := minCol; col <= maxCol; col++ {
			_, inside := b.fire(position{row, col})
			if inside {
				sum++
			}
		}
	}

	return sum
}

package main

import (
	"io"
	"math"

	aoc "github.com/teivah/advent-of-code"
)

type Position struct {
	x int
	y int
	z int
}

type Brick struct {
	id   rune
	from Position
	to   Position
}

type Board struct {
	bricks      []*Brick
	cubes       map[Position]*Brick
	perRow      map[int][]*Brick
	minPosition Position
	maxPosition Position
}

func (b *Brick) positionsBelow() []Position {
	z := min(b.from.z, b.to.z)
	if z == 1 {
		return nil
	}

	var positions []Position
	for x := min(b.from.x, b.to.x); x <= max(b.from.x, b.to.x); x++ {
		for y := min(b.from.y, b.to.y); y <= max(b.from.y, b.to.y); y++ {
			positions = append(positions, Position{
				x: x,
				y: y,
				z: z - 1,
			})
		}
	}
	return positions
}

func (b *Brick) moveDown(board map[Position]*Brick) func() {
	var deleted []Position
	for x := min(b.from.x, b.to.x); x <= max(b.from.x, b.to.x); x++ {
		for y := min(b.from.y, b.to.y); y <= max(b.from.y, b.to.y); y++ {
			for z := min(b.from.z, b.to.z); z <= max(b.from.z, b.to.z); z++ {
				p1 := Position{x: x, y: y, z: z}
				p2 := Position{x: x, y: y, z: z - 1}
				delete(board, p1)
				deleted = append(deleted, p1)
				board[p2] = b
			}
		}
	}

	b.from.z--
	b.to.z--

	return func() {
		for _, p := range deleted {
			board[p] = b
			p.z--
			delete(board, p)
		}
		b.from.z++
		b.to.z++
	}
}

func (b *Brick) disintegrate(board map[Position]*Brick) ([]Position, func()) {
	var up []Position
	for x := min(b.from.x, b.to.x); x <= max(b.from.x, b.to.x); x++ {
		for y := min(b.from.y, b.to.y); y <= max(b.from.y, b.to.y); y++ {
			for z := min(b.from.z, b.to.z); z <= max(b.from.z, b.to.z); z++ {
				p1 := Position{x: x, y: y, z: z}
				p2 := Position{x: x, y: y, z: z + 1}
				delete(board, p1)
				up = append(up, p2)
			}
		}
	}
	return up, func() {
		for _, p := range up {
			p.z--
			board[p] = b
		}
	}
}

func (b Board) initialFall() {
	for z := b.minPosition.z; z <= b.maxPosition.z; z++ {
		for _, brick := range b.perRow[z] {
			for {
				positionsBelow := brick.positionsBelow()
				if len(positionsBelow) == 0 {
					break
				}

				isEmpty := true
				for _, p := range positionsBelow {
					if _, exists := b.cubes[p]; exists {
						isEmpty = false
						break
					}
				}

				if !isEmpty {
					break
				}

				brick.moveDown(b.cubes)
			}
		}
	}
}

func fs1(input io.Reader) int {
	board := parse(input)
	board.initialFall()

	res := 0
	for _, brick := range board.bricks {
		upPositions, clean := brick.disintegrate(board.cubes)
		safe := true
		for _, upPosition := range upPositions {
			if upBrick, exists := board.cubes[upPosition]; exists {
				belowPositions := upBrick.positionsBelow()
				fall := true
				for _, belowPosition := range belowPositions {
					if _, exists := board.cubes[belowPosition]; exists {
						fall = false
						break
					}
				}
				if fall {
					safe = false
					break
				}
			}
		}
		if safe {
			res++
		}
		clean()
	}

	return res
}

func parse(input io.Reader) Board {
	var bricks []*Brick
	board := make(map[Position]*Brick)
	perRow := make(map[int][]*Brick)
	minPosition := Position{
		x: math.MaxInt,
		y: math.MaxInt,
		z: math.MaxInt,
	}
	maxPosition := Position{
		x: 0,
		y: 0,
		z: 0,
	}

	lines := aoc.ReaderToStrings(input)
	for id, line := range lines {
		del := aoc.NewDelimiter(line, "~")
		from := parsePosition(del.GetString(0))
		to := parsePosition(del.GetString(1))

		brick := &Brick{
			id:   rune(id + 'A'),
			from: from,
			to:   to,
		}
		bricks = append(bricks, brick)

		minPosition.x = min(minPosition.x, from.x, to.x)
		minPosition.y = min(minPosition.y, from.y, to.y)
		minPosition.z = min(minPosition.z, from.z, to.z)
		maxPosition.x = max(maxPosition.x, from.x, to.x)
		maxPosition.y = max(maxPosition.y, from.y, to.y)
		maxPosition.z = max(maxPosition.z, from.z, to.z)

		key := min(from.z, to.z)
		perRow[key] = append(perRow[key], brick)

		for x := from.x; x <= to.x; x++ {
			for y := from.y; y <= to.y; y++ {
				for z := from.z; z <= to.z; z++ {
					board[Position{x: x, y: y, z: z}] = brick
				}
			}
		}
	}

	return Board{
		bricks:      bricks,
		cubes:       board,
		perRow:      perRow,
		minPosition: minPosition,
		maxPosition: maxPosition,
	}
}

func parsePosition(s string) Position {
	ints := aoc.NewDelimiter(s, ",").GetInts()
	return Position{
		x: ints[0],
		y: ints[1],
		z: ints[2],
	}
}

func fs2(input io.Reader) int {
	board := parse(input)
	board.initialFall()

	res := 0
	for _, brick := range board.bricks {
		res += whatIf(board, brick)
	}
	return res
}

func whatIf(board Board, brick *Brick) int {
	res := 0
	_, clean := brick.disintegrate(board.cubes)
	defer clean()

	for z := min(brick.from.z, brick.to.z); z <= board.maxPosition.z; z++ {
		for _, b := range board.perRow[z] {
			falling := false
			for {
				positionsBelow := b.positionsBelow()
				if len(positionsBelow) == 0 {
					break
				}

				isEmpty := true
				for _, p := range positionsBelow {
					if _, exists := board.cubes[p]; exists {
						isEmpty = false
						break
					}
				}

				if !isEmpty {
					break
				}

				defer b.moveDown(board.cubes)()
				falling = true
			}
			if falling {
				res++
			}
		}
	}

	return res
}

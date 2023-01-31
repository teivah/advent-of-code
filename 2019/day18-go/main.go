package main

import (
	"io"
	"strings"
	"unicode"

	lib "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	board := toBoard(lib.ReaderToStrings(input))
	return board.bfs()
}

type Key struct {
	visited  int
	position lib.Position
}

func toKey(position lib.Position, remainingKeys map[rune]bool) Key {
	v := 0
	for r := range remainingKeys {
		v += 1 << int(r-'a')
	}
	return Key{
		visited:  v,
		position: position,
	}
}

type Key4 struct {
	visited   int
	position0 lib.Position
	position1 lib.Position
	position2 lib.Position
	position3 lib.Position
}

func toKeys(positions []lib.Position, remainingKeys map[rune]bool) Key4 {
	v := 0
	for r := range remainingKeys {
		v += 1 << int(r-'a')
	}
	return Key4{
		visited:   v,
		position0: positions[0],
		position1: positions[1],
		position2: positions[2],
		position3: positions[3],
	}
}

func (b *Board) bfs() int {
	remainingKeys := make(map[rune]bool)
	for _, r := range b.keys {
		remainingKeys[r] = true
	}
	closedDoors := make(map[rune]bool)
	for r := range b.doors {
		closedDoors[r] = true
	}
	q := []Entry{
		{
			position:      b.position,
			remainingKeys: remainingKeys,
			closedDoors:   closedDoors,
			visited:       map[lib.Position]bool{b.position: true},
		},
	}

	global := make(map[Key]int)

	for len(q) != 0 {
		e := q[0]
		q = q[1:]

		k := toKey(e.position, e.remainingKeys)
		if distance, exists := global[k]; exists {
			if distance <= e.distance {
				continue
			}
		}
		global[k] = e.distance

		r := b.grid[e.position]
		if isKey(r) {
			if e.remainingKeys[r] {
				delete(e.remainingKeys, r)
				if len(e.remainingKeys) == 0 {
					return e.distance
				}

				delete(e.closedDoors, unicode.ToUpper(r))
				e.visited = map[lib.Position]bool{e.position: true}
			}
		} else if isDoor(r) {
			if e.closedDoors[r] {
				continue
			}
		}

		pos := e.position.Delta(-1, 0)
		if b.exists(pos) && !e.visited[pos] {
			e := e.new(pos)
			e.moveUntilIntersection(b, lib.Up)
			q = append(q, e)
		}

		pos = e.position.Delta(1, 0)
		if b.exists(pos) && !e.visited[pos] {
			e := e.new(pos)
			e.moveUntilIntersection(b, lib.Down)
			q = append(q, e)
		}

		pos = e.position.Delta(0, -1)
		if b.exists(pos) && !e.visited[pos] {
			e := e.new(pos)
			e.moveUntilIntersection(b, lib.Left)
			q = append(q, e)
		}

		pos = e.position.Delta(0, 1)
		if b.exists(pos) && !e.visited[pos] {
			e := e.new(pos)
			e.moveUntilIntersection(b, lib.Right)
			q = append(q, e)
		}
	}

	return -1
}

func (b *Board) bfs4() int {
	remainingKeys := make(map[rune]bool)
	for _, r := range b.keys {
		remainingKeys[r] = true
	}
	closedDoors := make(map[rune]bool)
	for r := range b.doors {
		closedDoors[r] = true
	}
	visited := make(map[lib.Position]bool)
	for _, position := range b.positions {
		visited[position] = true
	}
	e4 := Entry4{
		positions:     b.positions,
		remainingKeys: remainingKeys,
		closedDoors:   closedDoors,
		visited:       visited,
	}
	q := []Entry4{e4}

	global := make(map[Key4]int)

	for len(q) != 0 {
		e := q[0]
		q = q[1:]

		k := toKeys(e.positions, e.remainingKeys)
		if distance, exists := global[k]; exists {
			if distance <= e.distance {
				continue
			}
		}
		global[k] = e.distance

		/*
			0 [row=3, col=5 row=3, col=7 row=5, col=5 row=5, col=7]
			1 [row=2, col=5 row=3, col=7 row=5, col=5 row=5, col=7]
			3 [row=2, col=5 row=1, col=7 row=5, col=5 row=5, col=7]
			5 [row=2, col=5 row=1, col=9 row=5, col=5 row=5, col=7]
			6 [row=2, col=5 row=1, col=9 row=5, col=5 row=6, col=7]
			7 [row=2, col=5 row=1, col=9 row=5, col=5 row=7, col=7]
			8 [row=3, col=5 row=1, col=9 row=5, col=5 row=7, col=7]
		*/
		//fmt.Println(e.distance, e.positions)

		for posID := 0; posID < len(e.positions); posID++ {
			pos := e.positions[posID].Delta(-1, 0)
			if b.exists(pos) && !e.visited[pos] {
				e := e.new(posID, pos)
				e.moveUntilIntersectionOrKeyOrDoor(posID, b, lib.Up)

				r := b.grid[e.positions[posID]]
				if isKey(r) {
					if e.remainingKeys[r] {
						delete(e.remainingKeys, r)
						if len(e.remainingKeys) == 0 {
							return e.distance
						}

						delete(e.closedDoors, unicode.ToUpper(r))
						e.visited = make(map[lib.Position]bool)
					}
				}
				if isDoor(r) && e.closedDoors[r] {

				} else {
					q = append(q, e)
				}
			}

			pos = e.positions[posID].Delta(1, 0)
			if b.exists(pos) && !e.visited[pos] {
				e := e.new(posID, pos)
				e.moveUntilIntersectionOrKeyOrDoor(posID, b, lib.Down)

				r := b.grid[e.positions[posID]]
				if isKey(r) {
					if e.remainingKeys[r] {
						delete(e.remainingKeys, r)
						if len(e.remainingKeys) == 0 {
							return e.distance
						}

						delete(e.closedDoors, unicode.ToUpper(r))
						e.visited = make(map[lib.Position]bool)
					}
				}
				if isDoor(r) && e.closedDoors[r] {

				} else {
					q = append(q, e)
				}
			}

			pos = e.positions[posID].Delta(0, -1)
			if b.exists(pos) && !e.visited[pos] {
				e := e.new(posID, pos)
				e.moveUntilIntersectionOrKeyOrDoor(posID, b, lib.Left)

				r := b.grid[e.positions[posID]]
				if isKey(r) {
					if e.remainingKeys[r] {
						delete(e.remainingKeys, r)
						if len(e.remainingKeys) == 0 {
							return e.distance
						}

						delete(e.closedDoors, unicode.ToUpper(r))
						e.visited = make(map[lib.Position]bool)
					}
				}
				if isDoor(r) && e.closedDoors[r] {

				} else {
					q = append(q, e)
				}
			}

			pos = e.positions[posID].Delta(0, 1)
			if b.exists(pos) && !e.visited[pos] {
				e := e.new(posID, pos)
				e.moveUntilIntersectionOrKeyOrDoor(posID, b, lib.Right)

				r := b.grid[e.positions[posID]]
				if isKey(r) {
					if e.remainingKeys[r] {
						delete(e.remainingKeys, r)
						if len(e.remainingKeys) == 0 {
							return e.distance
						}

						delete(e.closedDoors, unicode.ToUpper(r))
						e.visited = make(map[lib.Position]bool)
					}
				}
				if isDoor(r) && e.closedDoors[r] {

				} else {
					q = append(q, e)
				}
			}
		}
	}

	return -1
}

func (b *Board) exists(pos lib.Position) bool {
	_, exists := b.grid[pos]
	return exists
}

type Entry struct {
	position      lib.Position
	remainingKeys map[rune]bool
	closedDoors   map[rune]bool
	distance      int
	visited       map[lib.Position]bool
}

type Entry4 struct {
	positions     []lib.Position
	remainingKeys map[rune]bool
	closedDoors   map[rune]bool
	distance      int
	visited       map[lib.Position]bool
}

func (e *Entry) moveUntilIntersection(b *Board, dir lib.Direction) {
	for {
		if e.visited[e.position] {
			break
		}
		e.visited[e.position] = true
		if b.isIntersection(e.position) {
			break
		}

		r := b.grid[e.position]
		if isKey(r) || isDoor(r) {
			break
		}

		pos := e.position.Move(dir, 1)

		_, exists := b.grid[pos]
		if !exists {
			break
		}

		e.position = pos
		e.distance++
	}
}

func (e *Entry4) moveUntilIntersectionOrKeyOrDoor(i int, b *Board, dir lib.Direction) {
	for {
		if e.visited[e.positions[i]] {
			break
		}
		e.visited[e.positions[i]] = true
		if b.isIntersection(e.positions[i]) {
			break
		}

		r := b.grid[e.positions[i]]
		if isKey(r) || isDoor(r) {
			break
		}

		pos := e.positions[i].Move(dir, 1)

		_, exists := b.grid[pos]
		if !exists {
			break
		}

		e.positions[i] = pos
		e.distance++
	}
}

func (b *Board) isIntersection(pos lib.Position) bool {
	sum := 0
	if b.exists(pos.Delta(-1, 0)) {
		sum++
	}
	if b.exists(pos.Delta(1, 0)) {
		sum++
	}
	if b.exists(pos.Delta(0, -1)) {
		sum++
	}
	if b.exists(pos.Delta(0, 1)) {
		sum++
	}
	return sum > 2
}

func (e Entry4) new(i int, pos lib.Position) Entry4 {
	remainingKeys := make(map[rune]bool, len(e.remainingKeys))
	closedDoors := make(map[rune]bool, len(e.closedDoors))
	visited := make(map[lib.Position]bool, len(e.visited))
	for k, v := range e.remainingKeys {
		remainingKeys[k] = v
	}
	for k, v := range e.closedDoors {
		closedDoors[k] = v
	}
	for k, v := range e.visited {
		visited[k] = v
	}

	positions := make([]lib.Position, len(e.positions))
	for i, pos := range e.positions {
		positions[i] = pos
	}
	positions[i] = pos

	return Entry4{
		positions:     positions,
		remainingKeys: remainingKeys,
		closedDoors:   closedDoors,
		visited:       visited,
		distance:      e.distance + 1,
	}
}

func (e Entry) new(pos lib.Position) Entry {
	remainingKeys := make(map[rune]bool, len(e.remainingKeys))
	closedDoors := make(map[rune]bool, len(e.closedDoors))
	visited := make(map[lib.Position]bool, len(e.visited))
	for k, v := range e.remainingKeys {
		remainingKeys[k] = v
	}
	for k, v := range e.closedDoors {
		closedDoors[k] = v
	}
	for k, v := range e.visited {
		visited[k] = v
	}
	return Entry{
		position:      pos,
		remainingKeys: remainingKeys,
		closedDoors:   closedDoors,
		visited:       visited,
		distance:      e.distance + 1,
	}
}

type Board struct {
	grid      map[lib.Position]rune
	doors     map[rune]lib.Position
	keys      map[lib.Position]rune
	position  lib.Position
	positions []lib.Position
	maxKey    int
}

func toBoard(lines []string) *Board {
	grid := make(map[lib.Position]rune)
	doors := make(map[rune]lib.Position)
	keys := make(map[lib.Position]rune)
	row := -1
	board := &Board{}
	for _, line := range lines {
		line = strings.TrimSpace(line)
		row++
		for col := 0; col < len(line); col++ {
			r := rune(line[col])
			pos := lib.Position{row, col}
			if r == '#' {
				continue
			} else if r == '.' {
				grid[pos] = empty
			} else if r == '@' {
				board.position = pos
				grid[pos] = empty
			} else if isKey(r) {
				grid[pos] = r
				keys[pos] = r
				board.maxKey = lib.Max(board.maxKey, int(r-'a'))
			} else if isDoor(r) {
				grid[pos] = r
				doors[r] = pos
			} else {
				panic(r)
			}
		}
	}

	board.grid = grid
	board.doors = doors
	board.keys = keys

	return board
}

func toBoard4(lines []string) *Board {
	grid := make(map[lib.Position]rune)
	doors := make(map[rune]lib.Position)
	keys := make(map[lib.Position]rune)
	row := -1
	board := &Board{
		positions: make([]lib.Position, 4),
	}
	posID := 0
	for _, line := range lines {
		line = strings.TrimSpace(line)
		row++
		for col := 0; col < len(line); col++ {
			r := rune(line[col])
			pos := lib.Position{row, col}
			if r == '#' {
				continue
			} else if r == '.' {
				grid[pos] = empty
			} else if r == '@' {
				board.positions[posID] = pos
				posID++
				grid[pos] = empty
			} else if isKey(r) {
				grid[pos] = r
				keys[pos] = r
				board.maxKey = lib.Max(board.maxKey, int(r-'a'))
			} else if isDoor(r) {
				grid[pos] = r
				doors[r] = pos
			} else {
				panic(r)
			}
		}
	}

	board.grid = grid
	board.doors = doors
	board.keys = keys

	return board
}

func isKey(r rune) bool {
	return r >= 'a' && r <= 'z'
}

func isDoor(r rune) bool {
	return r >= 'A' && r <= 'Z'
}

const (
	empty rune = '.'
)

func fs2(input io.Reader) int {
	board := toBoard4(lib.ReaderToStrings(input))
	return board.bfs4()
}

package main

import (
	"bufio"
	"io"
	"strings"
	"unicode"

	lib "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	board := toBoard(lib.ReaderToStrings(input))
	return board.bfs()
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
			visited:       make(map[lib.Position]bool),
		},
	}

	for len(q) != 0 {
		e := q[0]
		q = q[1:]

		if e.visited[e.position] {
			continue
		}
		e.visited[e.position] = true

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

		up := e.position.Delta(-1, 0)
		if b.exists(up) && !e.visited[up] {
			q = append(q, e.new(up))
		}

		down := e.position.Delta(1, 0)
		if b.exists(down) && !e.visited[down] {
			q = append(q, e.new(down))
		}

		left := e.position.Delta(0, -1)
		if b.exists(left) && !e.visited[left] {
			q = append(q, e.new(left))
		}

		right := e.position.Delta(0, 1)
		if b.exists(right) && !e.visited[right] {
			q = append(q, e.new(right))
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
	grid     map[lib.Position]rune
	doors    map[rune]lib.Position
	keys     map[lib.Position]rune
	position lib.Position
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
	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
		line := scanner.Text()
		_ = line
	}

	return 42
}

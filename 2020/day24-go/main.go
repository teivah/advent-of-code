package main

import (
	"bufio"
	"io"

	aoc "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	var moves [][]Direction
	for scanner.Scan() {
		line := scanner.Text()
		moves = append(moves, toDirections(line))
	}

	g := Grid{blacks: make(map[aoc.Position]bool)}
	for _, move := range moves {
		g.flip(move)
	}

	sum := 0
	for _, isBlack := range g.blacks {
		if isBlack {
			sum++
		}
	}

	return sum
}

type Grid struct {
	blacks  map[aoc.Position]bool
	visited map[aoc.Position]bool
}

func (g *Grid) flip(directions []Direction) {
	current := aoc.Position{}
	for _, direction := range directions {
		switch direction {
		case west:
			current = current.Delta(-1, 0)
		case east:
			current = current.Delta(1, 0)
		case southWest:
			current = current.Delta(-1, 1)
		case southEast:
			current = current.Delta(0, 1)
		case northWest:
			current = current.Delta(0, -1)
		case northEast:
			current = current.Delta(1, -1)
		}
	}

	v := g.blacks[current]
	g.blacks[current] = !v
	g.expandAround(current)
}

func (g *Grid) expandAround(position aoc.Position) {
	pos := position.Delta(-1, 0)
	if _, exists := g.blacks[pos]; !exists {
		g.blacks[pos] = false
	}

	pos = position.Delta(1, 0)
	if _, exists := g.blacks[pos]; !exists {
		g.blacks[pos] = false
	}

	pos = position.Delta(-1, 1)
	if _, exists := g.blacks[pos]; !exists {
		g.blacks[pos] = false
	}

	pos = position.Delta(0, 1)
	if _, exists := g.blacks[pos]; !exists {
		g.blacks[pos] = false
	}

	pos = position.Delta(0, -1)
	if _, exists := g.blacks[pos]; !exists {
		g.blacks[pos] = false
	}

	pos = position.Delta(1, -1)
	if _, exists := g.blacks[pos]; !exists {
		g.blacks[pos] = false
	}
}

func toDirections(s string) []Direction {
	var directions []Direction
	for i := 0; i < len(s); i++ {
		r := rune(s[i])
		if r == 'e' {
			directions = append(directions, east)
		} else if r == 'w' {
			directions = append(directions, west)
		} else if r == 's' {
			r2 := rune(s[i+1])
			if r2 == 'e' {
				directions = append(directions, southEast)
			} else {
				directions = append(directions, southWest)
			}
			i++
		} else {
			r2 := rune(s[i+1])
			if r2 == 'e' {
				directions = append(directions, northEast)
			} else {
				directions = append(directions, northWest)
			}
			i++
		}
	}
	return directions
}

type Direction string

const (
	east      Direction = "e"
	southEast Direction = "se"
	southWest Direction = "sw"
	west      Direction = "w"
	northWest Direction = "nw"
	northEast Direction = "ne"
)

func fs2(input io.Reader, days int) int {
	scanner := bufio.NewScanner(input)
	var moves [][]Direction
	for scanner.Scan() {
		line := scanner.Text()
		moves = append(moves, toDirections(line))
	}

	g := Grid{blacks: make(map[aoc.Position]bool)}
	for _, move := range moves {
		g.flip(move)
	}

	for i := 0; i < days; i++ {
		blacks := make(map[aoc.Position]bool)
		g.visited = make(map[aoc.Position]bool)
		g.round(blacks, aoc.Position{}, false)
		g.blacks = blacks
	}

	sum := 0
	for _, isBlack := range g.blacks {
		if isBlack {
			sum++
		}
	}
	return sum
}

func (g *Grid) round(res map[aoc.Position]bool, position aoc.Position, expanded bool) {
	if g.visited[position] {
		return
	}
	g.visited[position] = true

	isBlack, exists := g.blacks[position]
	if !exists {
		return
	}

	count := g.countNeighbors(position)
	if isBlack {
		if count == 0 || count > 2 {
			res[position] = false
		} else {
			res[position] = true
		}
	} else {
		if count == 2 {
			res[position] = true
		} else {
			res[position] = false
		}
	}

	if expanded {
		return
	}

	pos := position.Delta(-1, 0)
	_, exists = g.blacks[pos]
	if !exists {
		g.blacks[pos] = false
	}
	g.round(res, pos, !exists)

	pos = position.Delta(1, 0)
	_, exists = g.blacks[pos]
	if !exists {
		g.blacks[pos] = false
	}
	g.round(res, pos, !exists)

	pos = position.Delta(-1, 1)
	_, exists = g.blacks[pos]
	if !exists {
		g.blacks[pos] = false
	}
	g.round(res, pos, !exists)

	pos = position.Delta(0, 1)
	_, exists = g.blacks[pos]
	if !exists {
		g.blacks[pos] = false
	}
	g.round(res, pos, !exists)

	pos = position.Delta(0, -1)
	_, exists = g.blacks[pos]
	if !exists {
		g.blacks[pos] = false
	}
	g.round(res, pos, !exists)

	pos = position.Delta(1, -1)
	_, exists = g.blacks[pos]
	if !exists {
		g.blacks[pos] = false
	}
	g.round(res, pos, !exists)
}

func (g *Grid) countNeighbors(position aoc.Position) int {
	return g.count(position.Delta(-1, 0)) +
		g.count(position.Delta(1, 0)) +
		g.count(position.Delta(-1, 1)) +
		g.count(position.Delta(0, 1)) +
		g.count(position.Delta(0, -1)) +
		g.count(position.Delta(1, -1))
}

func (g *Grid) count(position aoc.Position) int {
	if g.blacks[position] {
		return 1
	}
	return 0
}

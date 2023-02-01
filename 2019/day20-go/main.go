package main

import (
	"fmt"
	"io"

	lib "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	lines := lib.ReaderToStrings(input)

	var grid [][]rune
	for row, line := range lines {
		grid = append(grid, make([]rune, 0))
		for i := 0; i < len(line); i++ {
			r := rune(line[i])

			switch r {
			case ' ':
				grid[row] = append(grid[row], Empty)
			case '#':
				grid[row] = append(grid[row], Wall)
			case '.':
				grid[row] = append(grid[row], Tile)
			default:
				grid[row] = append(grid[row], r)
			}
		}
	}

	portalPositions := make(map[lib.Position]string)
	portals := make(map[string][]lib.Position)
	var start lib.Position
	var end lib.Position

	for row := 0; row < len(grid); row++ {
		for col := 0; col < len(grid[row]); col++ {
			r := grid[row][col]
			if !isLetter(r) {
				continue
			}

			// Down
			if row+2 < len(grid) {
				if isLetter(grid[row+1][col]) && grid[row+2][col] == Tile {
					name := fmt.Sprintf("%c%c", r, grid[row+1][col])
					position := lib.Position{row + 2, col}

					grid[row][col] = Empty
					grid[row+1][col] = Empty

					if name == "AA" {
						grid[row+2][col] = Tile
						start = position
					} else if name == "ZZ" {
						grid[row+2][col] = Tile
						end = position
					} else {
						grid[row+2][col] = Portal
						portals[name] = append(portals[name], position)
						portalPositions[position] = name
					}
				}
			}

			// Up
			if row >= 2 {
				if isLetter(grid[row-1][col]) && grid[row-2][col] == Tile {
					name := fmt.Sprintf("%c%c", grid[row-1][col], r)
					position := lib.Position{row - 2, col}

					grid[row][col] = Empty
					grid[row-1][col] = Empty
					if name == "AA" {
						grid[row-2][col] = Tile
						start = position
					} else if name == "ZZ" {
						grid[row-2][col] = Tile
						end = position
					} else {
						grid[row-2][col] = Portal
						portals[name] = append(portals[name], position)
						portalPositions[position] = name
					}
				}
			}

			// Left
			if col >= 2 {
				if isLetter(grid[row][col-1]) && grid[row][col-2] == Tile {
					name := fmt.Sprintf("%c%c", grid[row][col-1], r)
					position := lib.Position{row, col - 2}

					grid[row][col] = Empty
					grid[row][col-1] = Empty
					if name == "AA" {
						grid[row][col-2] = Tile
						start = position
					} else if name == "ZZ" {
						grid[row][col-2] = Tile
						end = position
					} else {
						grid[row][col-2] = Portal
						portals[name] = append(portals[name], position)
						portalPositions[position] = name
					}
				}
			}

			// Right
			if col+2 < len(grid[row]) {
				if isLetter(grid[row][col+1]) && grid[row][col+2] == Tile {
					name := fmt.Sprintf("%c%c", r, grid[row][col+1])
					position := lib.Position{row, col + 2}

					grid[row][col] = Empty
					grid[row][col+1] = Empty
					if name == "AA" {
						grid[row][col+2] = Tile
						start = position
					} else if name == "ZZ" {
						grid[row][col+2] = Tile
						end = position
					} else {
						grid[row][col+2] = Portal
						portals[name] = append(portals[name], position)
						portalPositions[position] = name
					}
				}
			}
		}
	}

	type State struct {
		pos      lib.Position
		distance int
	}
	q := []State{{pos: start, distance: 0}}
	visited := make(map[lib.Position]bool)

	for len(q) != 0 {
		s := q[0]
		q = q[1:]

		if s.pos == end {
			return s.distance
		}

		if visited[s.pos] {
			continue
		}
		visited[s.pos] = true

		positions := []lib.Position{
			s.pos.Delta(-1, 0),
			s.pos.Delta(1, 0),
			s.pos.Delta(0, 1),
			s.pos.Delta(0, -1),
		}
		for _, pos := range positions {
			if grid[pos.Row][pos.Col] == Tile {
				q = append(q, State{pos, s.distance + 1})
			} else if grid[pos.Row][pos.Col] == Portal {
				distance := s.distance + 2
				if portal, contains := portalPositions[pos]; contains {
					pos = getTeleportLocation(portal, portals, pos)
				} else {
					panic(pos)
				}
				q = append(q, State{pos, distance})
			}
		}
	}

	return -1
}

func getTeleportLocation(portal string, portals map[string][]lib.Position, pos lib.Position) lib.Position {
	a := portals[portal][0]
	b := portals[portal][1]
	if a == pos {
		return b
	}
	return a
}

func isLetter(r rune) bool {
	return r >= 'A' && r <= 'Z'
}

const (
	Wall   = '#'
	Empty  = ' '
	Tile   = '.'
	Portal = 'X'
)

func fs2(input io.Reader) int {
	lines := lib.ReaderToStrings(input)

	var grid [][]rune
	for row, line := range lines {
		grid = append(grid, make([]rune, 0))
		for i := 0; i < len(line); i++ {
			r := rune(line[i])

			switch r {
			case ' ':
				grid[row] = append(grid[row], Empty)
			case '#':
				grid[row] = append(grid[row], Wall)
			case '.':
				grid[row] = append(grid[row], Tile)
			default:
				grid[row] = append(grid[row], r)
			}
		}
	}

	portalPositions := make(map[lib.Position]string)
	portalLevels := make(map[lib.Position]int)
	portals := make(map[string][]lib.Position)
	var start lib.Position
	var end lib.Position

	for row := 0; row < len(grid); row++ {
		for col := 0; col < len(grid[row]); col++ {
			r := grid[row][col]
			if !isLetter(r) {
				continue
			}

			// Down
			if row+2 < len(grid) {
				if isLetter(grid[row+1][col]) && grid[row+2][col] == Tile {
					name := fmt.Sprintf("%c%c", r, grid[row+1][col])
					position := lib.Position{row + 2, col}

					grid[row][col] = Empty
					grid[row+1][col] = Empty

					if name == "AA" {
						grid[row+2][col] = Tile
						start = position
						portalPositions[position] = name
					} else if name == "ZZ" {
						grid[row+2][col] = Tile
						end = position
						portalPositions[position] = name
					} else {
						grid[row+2][col] = Tile
						portals[name] = append(portals[name], position)
						portalPositions[position] = name

						if row == 0 {
							portalLevels[position] = -1
						} else {
							portalLevels[position] = 1
						}
					}
				}
			}

			// Up
			if row >= 2 {
				if isLetter(grid[row-1][col]) && grid[row-2][col] == Tile {
					name := fmt.Sprintf("%c%c", grid[row-1][col], r)
					position := lib.Position{row - 2, col}

					grid[row][col] = Empty
					grid[row-1][col] = Empty
					if name == "AA" {
						grid[row-2][col] = Tile
						start = position
						portalPositions[position] = name
					} else if name == "ZZ" {
						grid[row-2][col] = Tile
						end = position
						portalPositions[position] = name
					} else {
						grid[row-2][col] = Tile
						portals[name] = append(portals[name], position)
						portalPositions[position] = name

						if row == len(grid)-1 {
							portalLevels[position] = -1
						} else {
							portalLevels[position] = 1
						}
					}
				}
			}

			// Left
			if col >= 2 {
				if isLetter(grid[row][col-1]) && grid[row][col-2] == Tile {
					name := fmt.Sprintf("%c%c", grid[row][col-1], r)
					position := lib.Position{row, col - 2}

					grid[row][col] = Empty
					grid[row][col-1] = Empty
					if name == "AA" {
						grid[row][col-2] = Tile
						start = position
						portalPositions[position] = name
					} else if name == "ZZ" {
						grid[row][col-2] = Tile
						end = position
						portalPositions[position] = name
					} else {
						grid[row][col-2] = Tile
						portals[name] = append(portals[name], position)
						portalPositions[position] = name

						if col == len(grid[row])-1 {
							portalLevels[position] = -1
						} else {
							portalLevels[position] = 1
						}
					}
				}
			}

			// Right
			if col+2 < len(grid[row]) {
				if isLetter(grid[row][col+1]) && grid[row][col+2] == Tile {
					name := fmt.Sprintf("%c%c", r, grid[row][col+1])
					position := lib.Position{row, col + 2}

					grid[row][col] = Empty
					grid[row][col+1] = Empty
					if name == "AA" {
						grid[row][col+2] = Tile
						start = position
						portalPositions[position] = name
					} else if name == "ZZ" {
						grid[row][col+2] = Tile
						end = position
						portalPositions[position] = name
					} else {
						grid[row][col+2] = Tile
						portals[name] = append(portals[name], position)
						portalPositions[position] = name

						if col == 0 {
							portalLevels[position] = -1
						} else {
							portalLevels[position] = 1
						}
					}
				}
			}
		}
	}

	type State struct {
		pos      lib.Position
		distance int
		level    int
	}
	q := []State{{pos: start, distance: 0}}
	type entry struct {
		pos   lib.Position
		level int
	}
	visited := make(map[entry]bool)

	for len(q) != 0 {
		s := q[0]
		q = q[1:]

		if s.pos == end && s.level == 0 {
			return s.distance
		}

		e := entry{pos: s.pos, level: s.level}
		if visited[e] {
			continue
		}
		visited[e] = true

		positions := []lib.Position{
			s.pos.Delta(-1, 0),
			s.pos.Delta(1, 0),
			s.pos.Delta(0, 1),
			s.pos.Delta(0, -1),
		}
		for _, pos := range positions {
			if grid[pos.Row][pos.Col] == Tile {
				if name, contains := portalPositions[pos]; contains {
					if s.level == 0 {
						if name != "AA" && name != "ZZ" {
							// Is outer (-1)
							if portalLevels[pos] == -1 {
								continue
							}
						}
					} else {
						if name == "AA" || name == "ZZ" {
							continue
						}
					}

					distance := s.distance + 2
					p := pos
					if portal, contains := portalPositions[pos]; contains {
						if name != "AA" && name != "ZZ" {
							pos = getTeleportLocation(portal, portals, pos)
						} else {
							distance--
						}
					} else {
						panic(pos)
					}
					q = append(q, State{pos, distance, s.level + portalLevels[p]})
				} else {
					q = append(q, State{pos, s.distance + 1, s.level})
				}
			}
		}
	}

	return -1
}

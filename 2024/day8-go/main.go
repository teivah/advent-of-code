package main

import (
	"io"

	"github.com/teivah/go-aoc"
)

type cell struct {
	empty     bool
	frequency rune
}

func fs1(input io.Reader) int {
	antennas := make(map[rune][]aoc.Position)
	board := aoc.ParseBoard(aoc.ReaderToStrings(input), func(r rune, pos aoc.Position) cell {
		switch r {
		case '.':
			return cell{empty: true}
		default:
			antennas[r] = append(antennas[r], pos)
			return cell{frequency: r}
		}
	})

	antinodes := make(map[aoc.Position]struct{})
	for _, positions := range antennas {
		findAntinodes(board, positions, antinodes, setAntinode)
	}

	return len(antinodes)
}

func findAntinodes(board aoc.Board[cell], positions []aoc.Position, antinodes map[aoc.Position]struct{}, f func(board aoc.Board[cell], x, y aoc.Position, antinodes map[aoc.Position]struct{})) {
	for i := 0; i < len(positions); i++ {
		for j := i + 1; j < len(positions); j++ {
			x := positions[i]
			y := positions[j]
			f(board, x, y, antinodes)
			f(board, y, x, antinodes)
		}
	}
}

func setAntinode(board aoc.Board[cell], x, y aoc.Position, antinodes map[aoc.Position]struct{}) {
	d := delta(x, y)
	z := delta(y, d)
	if board.Contains(z) {
		antinodes[z] = struct{}{}
	}
}

func delta(x, y aoc.Position) aoc.Position {
	return aoc.Position{
		Row: x.Row - y.Row,
		Col: x.Col - y.Col,
	}
}

func fs2(input io.Reader) int {
	antennas := make(map[rune][]aoc.Position)
	board := aoc.ParseBoard(aoc.ReaderToStrings(input), func(r rune, pos aoc.Position) cell {
		switch r {
		case '.':
			return cell{empty: true}
		default:
			antennas[r] = append(antennas[r], pos)
			return cell{frequency: r}
		}
	})

	antinodes := make(map[aoc.Position]struct{})
	for _, positions := range antennas {
		if len(positions) <= 1 {
			continue
		}
		findAntinodes(board, positions, antinodes, setAntinode2)
		for _, pos := range positions {
			antinodes[pos] = struct{}{}
		}
	}

	return len(antinodes)
}

func setAntinode2(board aoc.Board[cell], x, y aoc.Position, antinodes map[aoc.Position]struct{}) {
	d := delta(x, y)
	z := delta(y, d)
	if board.Contains(z) {
		antinodes[z] = struct{}{}
		setAntinode2(board, y, z, antinodes)
	}
}

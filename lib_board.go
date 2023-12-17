package aoc

import "io"

// ParseBoard parses a board and maps it to a map of Position.
func ParseBoard[T any](input io.Reader, fn func(r rune) T) (board map[Position]T, rows int, cols int) {
	lines := ReaderToStrings(input)
	board = make(map[Position]T, len(lines)*len(lines[0]))
	for row, line := range lines {
		runes := []rune(line)
		for col, c := range runes {
			board[NewPosition(row, col)] = fn(c)
		}
	}
	return board, len(lines), len(lines[0])
}

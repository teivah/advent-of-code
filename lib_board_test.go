package aoc

import (
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestParseBoard(t *testing.T) {
	reader := strings.NewReader(`01
23`)
	board, rows, cols := ParseBoard[int](reader, func(r rune) int {
		return int(r - '0')
	})
	assert.Equal(t, 0, board[NewPosition(0, 0)])
	assert.Equal(t, 1, board[NewPosition(0, 1)])
	assert.Equal(t, 2, board[NewPosition(1, 0)])
	assert.Equal(t, 3, board[NewPosition(1, 1)])
	assert.Equal(t, 2, rows)
	assert.Equal(t, 2, cols)
}

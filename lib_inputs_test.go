package aoc_test

import (
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	aoc "github.com/teivah/advent-of-code"
)

func TestDelimiter(t *testing.T) {
	del := aoc.NewDelimiter("a b c", " ")
	assert.Equal(t, []string{"a", "b", "c"}, del.GetStrings())

	del = aoc.NewDelimiter("a, b, c", ", ")
	assert.Equal(t, []string{"a", "b", "c"}, del.GetStrings())

	del = aoc.NewDelimiter("", " ")
	assert.Equal(t, 0, len(del.GetStrings()))

	del = aoc.NewDelimiter("a", " ")
	assert.Equal(t, []string{"a"}, del.GetStrings())

	del = aoc.NewDelimiter("0;1; 2", ";", aoc.WithTrimSpace())
	assert.Equal(t, []int{0, 1, 2}, del.GetInts())

	del = aoc.NewDelimiter("0   1 2  ", " ", aoc.WithTrimSpace())
	assert.Equal(t, []int{0, 1, 2}, del.GetInts())
}

func TestSubstring(t *testing.T) {
	assert.Equal(t, "foo", aoc.Substring("id 4: foo", ": "))
}

func TestStringGroups(t *testing.T) {
	assert.Equal(t, [][]string{{"foo"}, {"bar", "baz"}}, aoc.StringGroups([]string{"foo", "", "bar", "baz"}))
}

func TestParseBoard(t *testing.T) {
	inputStr := `01
23`
	reader := strings.NewReader(inputStr)
	board := aoc.ParseBoard[int](aoc.ReaderToStrings(reader), func(r rune, _ aoc.Position) int {
		return int(r - '0')
	})
	assert.Equal(t, 0, board.Get(aoc.NewPosition(0, 0)))
	assert.Equal(t, 1, board.Get(aoc.NewPosition(0, 1)))
	assert.Equal(t, 2, board.Get(aoc.NewPosition(1, 0)))
	assert.Equal(t, 3, board.Get(aoc.NewPosition(1, 1)))
	assert.Equal(t, 2, board.MaxRows)
	assert.Equal(t, 2, board.MaxCols)

	assert.Equal(t, inputStr+"\n", board.String(aoc.IntToRune, '?'))
}

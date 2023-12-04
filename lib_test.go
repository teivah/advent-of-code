package aoc_test

import (
	"testing"

	"github.com/stretchr/testify/assert"
	aoc "github.com/teivah/advent-of-code"
)

func TestDelimiter_GetStrings(t *testing.T) {
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
}

func TestSubstring(t *testing.T) {
	assert.Equal(t, "foo", aoc.Substring("id 4: foo", ": "))
}

func TestStringGroups(t *testing.T) {
	assert.Equal(t, [][]string{{"foo"}, {"bar", "baz"}}, aoc.StringGroups([]string{"foo", "", "bar", "baz"}))
}

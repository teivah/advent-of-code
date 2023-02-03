package aoc

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestDelimiter_GetStrings(t *testing.T) {
	del := NewDelimiter("a b c", " ")
	assert.Equal(t,[]string{"a", "b", "c"}, del.GetStrings())

	del = NewDelimiter("a, b, c", ", ")
	assert.Equal(t,[]string{"a", "b", "c"}, del.GetStrings())

	del = NewDelimiter("", " ")
	assert.Equal(t,0, len(del.GetStrings()))

	del = NewDelimiter("a", " ")
	assert.Equal(t,[]string{"a"}, del.GetStrings())
}
package aoc_test

import (
	"slices"
	"strconv"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	aoc "github.com/teivah/advent-of-code"
)

func TestMapToSlice(t *testing.T) {
	m := map[int]string{
		1: "1",
		2: "2",
		3: "3",
	}
	s1 := aoc.MapKeysToSlice(m)
	s2 := aoc.MapValuesToSlice(m)
	require.Equal(t, len(s1), len(m))
	require.Equal(t, len(s2), len(m))
	for k, v := range m {
		assert.True(t, slices.Contains(s1, k))
		assert.True(t, slices.Contains(s2, v))
	}
}

func TestSliceToMap(t *testing.T) {
	m := aoc.SliceToMap([]int{1, 2, 3}, func(k int) string {
		return strconv.Itoa(k)
	})
	assert.Equal(t, map[int]string{
		1: "1",
		2: "2",
		3: "3",
	}, m)
}

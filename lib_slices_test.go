package aoc

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestSliceCopy(t *testing.T) {
	a := []int{1, 2, 3}
	b := SliceCopy(a)
	assert.Equal(t, a, b)
}

func TestSliceWithoutIndex(t *testing.T) {
	a := []int{1, 2, 3, 4}
	assert.Equal(t, []int{2, 3, 4}, SliceWithoutIndex(a, 0))
	assert.Equal(t, []int{1, 2, 3}, SliceWithoutIndex(a, 3))
	assert.Equal(t, []int{1, 3, 4}, SliceWithoutIndex(a, 1))
}

func TestSliceSorted(t *testing.T) {
	assert.Equal(t, true, IsSliceMonotonicallyIncreasing([]int{1, 2, 2}))
	assert.Equal(t, false, IsSliceStrictlyIncreasing([]int{1, 2, 2}))
	assert.Equal(t, true, IsSliceStrictlyIncreasing([]int{1, 2, 3}))
}

func TestCountSliceOccurrence(t *testing.T) {
	assert.Equal(t, map[int]int{1: 1, 2: 2}, CountSliceOccurrence([]int{2, 1, 2}))
}

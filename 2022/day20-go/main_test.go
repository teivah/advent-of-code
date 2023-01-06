package main

import (
	"os"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestRing(t *testing.T) {
	testMove(t, []int{2, 1, 3, 4}, 0, []int{1, 3, 2, 4})
	testMove(t, []int{2, 1, -2, 8}, 2, []int{-2, 2, 1, 8})
	testMove(t, []int{1, 2, 3, -2, 4}, 3, []int{1, -2, 2, 3, 4})
	testMove(t, []int{1, 2, 3, 4, 5}, 2, []int{1, 3, 2, 4, 5})
	testMove(t, []int{1, 3, 2, 4, 5}, 2, []int{2, 1, 3, 4, 5})
	testMove(t, []int{10, 5, 1, 3}, 2, []int{1, 10, 5, 3})
	testMove(t, []int{0, 3, 1, 10}, 1, []int{0, 3, 1, 10})
	testMove(t, []int{0, 6, 1, 10}, 1, []int{0, 6, 1, 10})
	testMove(t, []int{1, -3, 2, 3}, 1, []int{1, -3, 2, 3})
	testMove(t, []int{1, -6, 2, 3}, 1, []int{1, -6, 2, 3})
	testMove(t, []int{1, -8, 2, 3}, 1, []int{1, 2, -8, 3})
	testMove(t, []int{1, 8, 2, 3}, 1, []int{8, 1, 2, 3})
	testMove(t, []int{1, 10, 2, 3}, 1, []int{1, 2, 10, 3})
	testMove(t, []int{1, 13, 2, 3}, 1, []int{1, 2, 13, 3})
}

func testMove(t *testing.T, from []int, move int, to []int) {
	r := NewRing(from)
	r.move(move)
	assert.Equal(t, to, r.getData())
}

func TestFs1Test(t *testing.T) {
	f, err := os.Open("test.txt")
	require.NoError(t, err)
	v, err := fs1(f, []int{1000, 2000, 3000})
	require.NoError(t, err)
	assert.Equal(t, 3, v)
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	v, err := fs1(f, []int{1000, 2000, 3000})
	require.NoError(t, err)
	assert.Equal(t, 3346, v)
}

func TestFs2Test(t *testing.T) {
	f, err := os.Open("test.txt")
	require.NoError(t, err)
	v, err := fs2(f, []int{1000, 2000, 3000}, 811589153, 10)
	require.NoError(t, err)
	assert.Equal(t, 1623178306, v)
}

func TestFs2Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	v, err := fs2(f, []int{1000, 2000, 3000}, 811589153, 10)
	require.NoError(t, err)
	assert.Equal(t, 4265712588168, v)
}

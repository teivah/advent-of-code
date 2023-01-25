package main

import (
	"os"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestFs1Test(t *testing.T) {
	f, err := os.Open("test.txt")
	require.NoError(t, err)
	assert.Equal(t, 6, fs1(f, make([]int, 6)))
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 2072, fs1(f, make([]int, 6)))
}

func TestFs2Input(t *testing.T) {
	//assert.Equal(t, 42, fs2())
	f, err := os.Open("test.txt")
	require.NoError(t, err)
	assert.Equal(t, 6, fs1(f, []int{1, 0, 0, 0, 0, 0}))
}

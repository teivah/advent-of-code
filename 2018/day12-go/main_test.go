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
	assert.Equal(t, 325, fs1(f, 20))
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 42, fs1(f, 20))
}

func TestFs2Input(t *testing.T) {
	assert.Equal(t, 3650000001776, generationScore(50000000000))
}

func Test_transform(t *testing.T) {
	s := ".#.##."
	assert.Equal(t, ".#..#.", transform(s, 1, "."))
}

func Test_newState(t *testing.T) {
	assert.Equal(t, "...........", newState("...#####...", map[string]string{"#####": "."}))
}

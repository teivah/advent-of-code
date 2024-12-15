package main

import (
	"os"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestFs1TestSmall(t *testing.T) {
	f, err := os.Open("test_small.txt")
	require.NoError(t, err)
	assert.Equal(t, 2028, fs1(f))
}

func TestFs1TestLarge(t *testing.T) {
	f, err := os.Open("test_large.txt")
	require.NoError(t, err)
	assert.Equal(t, 10092, fs1(f))
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 1383666, fs1(f))
}

func TestFs2Test(t *testing.T) {
	f, err := os.Open("test_large.txt")
	require.NoError(t, err)
	assert.Equal(t, 9021, fs2(f))
}

func TestFs2Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 1412866, fs2(f))
}

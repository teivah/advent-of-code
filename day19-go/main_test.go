package main

import (
	"os"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestFs1Unit(t *testing.T) {
	f, err := os.Open("test.txt")
	require.NoError(t, err)
	v, err := fs1(f, 22)
	require.NoError(t, err)
	assert.Equal(t, 17, v)
}

func TestFs1Test(t *testing.T) {
	f, err := os.Open("test.txt")
	require.NoError(t, err)
	v, err := fs1(f, 24)
	require.NoError(t, err)
	assert.Equal(t, 33, v)
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	v, err := fs1(f, 24)
	require.NoError(t, err)
	assert.Equal(t, 1147, v)
}

func TestFs2Test(t *testing.T) {
	f, err := os.Open("test.txt")
	require.NoError(t, err)
	v, err := fs2(f, 32, 3)
	require.NoError(t, err)
	assert.Equal(t, 24933642, v)
}

func TestFs2Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	v, err := fs2(f, 32, 3)
	require.NoError(t, err)
	assert.Equal(t, 4370655, v)
}

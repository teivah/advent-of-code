package main

import (
	"os"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestFs1Test1(t *testing.T) {
	f, err := os.Open("test1.txt")
	require.NoError(t, err)
	assert.Equal(t, 7036, fs1(f))
}

func TestFs1Test2(t *testing.T) {
	f, err := os.Open("test2.txt")
	require.NoError(t, err)
	assert.Equal(t, 11048, fs1(f))
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 89460, fs1(f))
}

func TestFs2Test1(t *testing.T) {
	f, err := os.Open("test1.txt")
	require.NoError(t, err)
	assert.Equal(t, 45, fs2(f))
}

func TestFs2Test2(t *testing.T) {
	f, err := os.Open("test2.txt")
	require.NoError(t, err)
	assert.Equal(t, 64, fs2(f))
}

func TestFs2Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 504, fs2(f))
}

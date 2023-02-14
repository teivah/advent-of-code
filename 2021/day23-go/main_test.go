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
	assert.Equal(t, 12521, fs(f))
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 19167, fs(f))
}

func TestFs2Test(t *testing.T) {
	f, err := os.Open("test2.txt")
	require.NoError(t, err)
	assert.Equal(t, 44169, fs(f))
}

func TestFs2Input(t *testing.T) {
	f, err := os.Open("input2.txt")
	require.NoError(t, err)
	assert.Equal(t, 47665, fs(f))
}

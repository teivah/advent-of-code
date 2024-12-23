package main

import (
	"os"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestMix(t *testing.T) {
	assert.Equal(t, 37, mix(42, 15))
}

func TestPrune(t *testing.T) {
	assert.Equal(t, 16113920, prune(100000000))
}

func TestFs1Test(t *testing.T) {
	f, err := os.Open("test.txt")
	require.NoError(t, err)
	assert.Equal(t, 37327623, fs1(f, 2000))
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 14726157693, fs1(f, 2000))
}

func TestFs2Test(t *testing.T) {
	f, err := os.Open("test2.txt")
	require.NoError(t, err)
	assert.Equal(t, 23, fs2(f, 2001))
}

func TestFs2Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 1614, fs2(f, 2001))
}

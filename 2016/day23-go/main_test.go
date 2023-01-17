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
	v, err := fs1(f, map[string]int16{})
	require.NoError(t, err)
	assert.Equal(t, int16(3), v)
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	v, err := fs1(f, map[string]int16{"a": 7})
	require.NoError(t, err)
	assert.Equal(t, int16(10807), v)
}

func TestFs2Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)

	v, err := fs2(f, map[string]int16{"a": 12})
	require.NoError(t, err)
	assert.Equal(t, int16(332), v)
}

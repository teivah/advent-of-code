package main

import (
	"os"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestFs1Unit(t *testing.T) {
	f, err := os.Open("unit.txt")
	require.NoError(t, err)
	v, err := fs1(f, 5)
	require.NoError(t, err)
	assert.Equal(t, 9, v)
}

func TestFs1Test(t *testing.T) {
	f, err := os.Open("test.txt")
	require.NoError(t, err)
	v, err := fs1(f, 12)
	require.NoError(t, err)
	assert.Equal(t, 18, v)
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	v, err := fs1(f, 300)
	require.NoError(t, err)
	assert.Equal(t, 373, v)
}

func TestFs2Test(t *testing.T) {
	f, err := os.Open("test.txt")
	require.NoError(t, err)
	v, err := fs2(f, 12)
	require.NoError(t, err)
	assert.Equal(t, 54, v)
}

func TestFs2Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	v, err := fs2(f, 300)
	require.NoError(t, err)
	assert.Equal(t, 997, v)
}

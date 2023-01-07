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
	v, err := fs1(f, "f")
	require.NoError(t, err)
	assert.Equal(t, 492, v)
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	v, err := fs1(f, "a")
	require.NoError(t, err)
	assert.Equal(t, 42, v)
}

func TestFs2Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	v, err := fs2(f, "a", "b")
	require.NoError(t, err)
	assert.Equal(t, 42, v)
}

func TestIndexAll(t *testing.T) {
	assert.Equal(t, []int{0, 5, 10}, indexAll("abxxxabiiiabxx", "ab"))
}

package main

import (
	"os"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestFs1Test(t *testing.T) {
	t.Run("test1", func(t *testing.T) {
		f, err := os.Open("test.txt")
		require.NoError(t, err)
		assert.Equal(t, 32000000, fs1(f, 1000))
	})
	t.Run("test2", func(t *testing.T) {
		f, err := os.Open("test2.txt")
		require.NoError(t, err)
		assert.Equal(t, 11687500, fs1(f, 1000))
	})
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 730797576, fs1(f, 1000))
}

func TestFs2Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 226732077152351, fs2(f))
}

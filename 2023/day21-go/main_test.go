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
	assert.Equal(t, 16, fs1(f, 6))
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 3841, fs1(f, 64))
}

func TestFs2Test(t *testing.T) {
	t.Run("6", func(t *testing.T) {
		f, err := os.Open("test.txt")
		require.NoError(t, err)
		assert.Equal(t, 16, fs2(f, 6))
	})
	t.Run("10", func(t *testing.T) {
		f, err := os.Open("test.txt")
		require.NoError(t, err)
		assert.Equal(t, 50, fs2(f, 10))
	})
	t.Run("50", func(t *testing.T) {
		f, err := os.Open("test.txt")
		require.NoError(t, err)
		assert.Equal(t, 1594, fs2(f, 50))
	})
	t.Run("100", func(t *testing.T) {
		f, err := os.Open("test.txt")
		require.NoError(t, err)
		assert.Equal(t, 6536, fs2(f, 100))
	})
	t.Run("500", func(t *testing.T) {
		f, err := os.Open("test.txt")
		require.NoError(t, err)
		assert.Equal(t, 167004, fs2(f, 500))
	})
	t.Run("1000", func(t *testing.T) {
		f, err := os.Open("test.txt")
		require.NoError(t, err)
		assert.Equal(t, 668697, fs2(f, 1000))
	})
	t.Run("5000", func(t *testing.T) {
		f, err := os.Open("test.txt")
		require.NoError(t, err)
		assert.Equal(t, 16733044, fs2(f, 5000))
	})
}

func TestFs2Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 42, fs2(f, 26501365))
}

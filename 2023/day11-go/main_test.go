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
	assert.Equal(t, 374, fs(f, 2))
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 10885634, fs(f, 2))
}

func TestFs2Test(t *testing.T) {
	t.Run("10", func(t *testing.T) {
		f, err := os.Open("test.txt")
		require.NoError(t, err)
		assert.Equal(t, 1030, fs(f, 10))
	})
	t.Run("100", func(t *testing.T) {
		f, err := os.Open("test.txt")
		require.NoError(t, err)
		assert.Equal(t, 8410, fs(f, 100))
	})
}

func TestFs2Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 707505470642, fs(f, 1000000))
}

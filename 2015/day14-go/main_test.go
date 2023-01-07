package main

import (
	"os"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestFs1Unit(t *testing.T) {
	v, err := fs1(strings.NewReader("Dancer can fly 4 km/s for 2 seconds, but then must rest for 3 seconds."), 11)
	require.NoError(t, err)
	assert.Equal(t, 20, v)
}

func TestFs1Test(t *testing.T) {
	f, err := os.Open("test.txt")
	require.NoError(t, err)
	v, err := fs1(f, 1000)
	require.NoError(t, err)
	assert.Equal(t, 1120, v)
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	v, err := fs1(f, 2503)
	require.NoError(t, err)
	assert.Equal(t, 2696, v)
}

func TestFs2Test(t *testing.T) {
	f, err := os.Open("test.txt")
	require.NoError(t, err)
	v, err := fs2(f, 1000)
	require.NoError(t, err)
	assert.Equal(t, 689, v)
}

func TestFs2Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	v, err := fs2(f, 2503)
	require.NoError(t, err)
	assert.Equal(t, 1084, v)
}

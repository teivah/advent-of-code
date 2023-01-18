package main

import (
	"os"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestUnit(t *testing.T) {
	v, err := fs1(strings.NewReader(`b dec 5 if a == 0`))
	require.NoError(t, err)
	assert.Equal(t, -5, v)
}

func TestFs1Test(t *testing.T) {
	f, err := os.Open("test.txt")
	require.NoError(t, err)
	v, err := fs1(f)
	require.NoError(t, err)
	assert.Equal(t, 1, v)
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	v, err := fs1(f)
	require.NoError(t, err)
	assert.Equal(t, 2968, v)
}

func TestFs2Test(t *testing.T) {
	f, err := os.Open("test.txt")
	require.NoError(t, err)
	v, err := fs2(f)
	require.NoError(t, err)
	assert.Equal(t, 10, v)
}

func TestFs2Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	v, err := fs2(f)
	require.NoError(t, err)
	assert.Equal(t, 7491, v)
}

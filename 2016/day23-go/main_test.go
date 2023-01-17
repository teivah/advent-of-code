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
	v, err := fs1(f, map[string]int32{})
	require.NoError(t, err)
	assert.Equal(t, int32(3), v)
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	v, err := fs1(f, map[string]int32{"a": 7})
	require.NoError(t, err)
	assert.Equal(t, int32(10807), v)
}

func TestFs1Input2(t *testing.T) {
	f, err := os.Open("input2.txt")
	require.NoError(t, err)
	v, err := fs1(f, map[string]int32{"a": 12})
	require.NoError(t, err)
	assert.Equal(t, int32(479007367), v)
}

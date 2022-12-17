package day15_go

import (
	"io"
	"os"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func fileReader(t *testing.T, s string) io.Reader {
	f, err := os.Open(s)
	require.NoError(t, err)
	return f
}

func TestUnit1(t *testing.T) {
	v, err := fn1(fileReader(t, "test.txt"), 2022)
	require.NoError(t, err)
	assert.Equal(t, 3068, v)
}

func TestInput1(t *testing.T) {
	v, err := fn1(fileReader(t, "input.txt"), 2022)
	require.NoError(t, err)
	assert.Equal(t, 3166, v)
}

func TestUnit2(t *testing.T) {
	v, err := fn2(fileReader(t, "test.txt"), 1000000000000)
	require.NoError(t, err)
	assert.Equal(t, 3068, v)
}

func TestInput2(t *testing.T) {
	v, err := fn2(fileReader(t, "input.txt"), 1000000000000)
	require.NoError(t, err)
	assert.Equal(t, 3068, v)
}

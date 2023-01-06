package main

import (
	"os"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestFs1UnitTest(t *testing.T) {
	run(t, `
.#
##`, 4)
}

func run(t *testing.T, s string, count int) {
	_, err := fs1(
		strings.NewReader(s), count,
	)
	require.NoError(t, err)
}

func TestFs1Test(t *testing.T) {
	f, err := os.Open("test.txt")
	require.NoError(t, err)
	v, err := fs1(f, 10)
	require.NoError(t, err)
	assert.Equal(t, 110, v)
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	v, err := fs1(f, 10)
	require.NoError(t, err)
	assert.Equal(t, 4045, v)
}

func TestFs2Test(t *testing.T) {
	f, err := os.Open("test.txt")
	require.NoError(t, err)
	v, err := fs2(f)
	require.NoError(t, err)
	assert.Equal(t, 20, v)
}

func TestFs2Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	v, err := fs2(f)
	require.NoError(t, err)
	assert.Equal(t, 963, v)
}

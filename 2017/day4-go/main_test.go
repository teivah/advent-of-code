package main

import (
	"os"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestFs1Test(t *testing.T) {
	v, err := fs1(strings.NewReader("aa bb cc dd ee"))
	require.NoError(t, err)
	assert.Equal(t, 1, v)

	v, err = fs1(strings.NewReader("aa bb cc dd aa"))
	require.NoError(t, err)
	assert.Equal(t, 0, v)

	v, err = fs1(strings.NewReader("aa bb cc dd aaa"))
	require.NoError(t, err)
	assert.Equal(t, 1, v)
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	v, err := fs1(f)
	require.NoError(t, err)
	assert.Equal(t, 42, v)
}

func TestFs2Test(t *testing.T) {
	v, err := fs2(strings.NewReader("abcde fghij"))
	require.NoError(t, err)
	assert.Equal(t, 1, v)

	v, err = fs2(strings.NewReader("abcde xyz ecdab"))
	require.NoError(t, err)
	assert.Equal(t, 0, v)

	v, err = fs2(strings.NewReader("a ab abc abd abf abj"))
	require.NoError(t, err)
	assert.Equal(t, 1, v)

	v, err = fs2(strings.NewReader("iiii oiii ooii oooi oooo"))
	require.NoError(t, err)
	assert.Equal(t, 1, v)

	v, err = fs2(strings.NewReader("oiii ioii iioi iiio"))
	require.NoError(t, err)
	assert.Equal(t, 0, v)
}

func TestFs2Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	v, err := fs2(f)
	require.NoError(t, err)
	assert.Equal(t, 42, v)
}

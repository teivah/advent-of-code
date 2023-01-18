package main

import (
	"os"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestFs1Test(t *testing.T) {
	assert.Equal(t, 1, fs1(strings.NewReader(`{}`)))
	assert.Equal(t, 6, fs1(strings.NewReader(`{{{}}}`)))
	assert.Equal(t, 5, fs1(strings.NewReader(`{{},{}}`)))
	assert.Equal(t, 16, fs1(strings.NewReader(`{{{},{},{{}}}}`)))
	assert.Equal(t, 1, fs1(strings.NewReader(`{<a>,<a>,<a>,<a>}`)))
	assert.Equal(t, 9, fs1(strings.NewReader(`{{<ab>},{<ab>},{<ab>},{<ab>}}`)))
	assert.Equal(t, 9, fs1(strings.NewReader(`{{<!!>},{<!!>},{<!!>},{<!!>}}`)))
	assert.Equal(t, 3, fs1(strings.NewReader(`{{<a!>},{<a!>},{<a!>},{<ab>}}`)))
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 16021, fs1(f))
}

func TestFs2Test(t *testing.T) {
	assert.Equal(t, 0, fs2(strings.NewReader(`<>`)))
	assert.Equal(t, 17, fs2(strings.NewReader(`<random characters>`)))
	assert.Equal(t, 3, fs2(strings.NewReader(`<<<<>`)))
	assert.Equal(t, 2, fs2(strings.NewReader(`<{!>}>`)))
	assert.Equal(t, 0, fs2(strings.NewReader(`<!!>`)))
	assert.Equal(t, 0, fs2(strings.NewReader(`<!!!>>`)))
	assert.Equal(t, 10, fs2(strings.NewReader(`<{o"i!a,<{i<a>`)))
}

func TestFs2Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 7685, fs2(f))
}

package main

import (
	"os"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestFs1Test(t *testing.T) {
	assert.Equal(t, 71, fs1(strings.NewReader(`1 + 2 * 3 + 4 * 5 + 6`)))
	assert.Equal(t, 51, fs1(strings.NewReader(`1 + (2 * 3) + (4 * (5 + 6))`)))
	assert.Equal(t, 26, fs1(strings.NewReader(`2 * 3 + (4 * 5)`)))
	assert.Equal(t, 437, fs1(strings.NewReader(`5 + (8 * 3 + 9 + 3 * 4 * 3)`)))
	assert.Equal(t, 12240, fs1(strings.NewReader(`5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))`)))
	assert.Equal(t, 13632, fs1(strings.NewReader(`((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2`)))
	assert.Equal(t, 21, fs1(strings.NewReader(`10 + 11`)))
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 21022630974613, fs1(f))
}

func TestFs2Test(t *testing.T) {
	assert.Equal(t, 3, fs2(strings.NewReader(`1 + 2`)))
	assert.Equal(t, 231, fs2(strings.NewReader(`1 + 2 * 3 + 4 * 5 + 6`)))
	assert.Equal(t, 51, fs2(strings.NewReader(`1 + (2 * 3) + (4 * (5 + 6))`)))
	assert.Equal(t, 46, fs2(strings.NewReader(`2 * 3 + (4 * 5)`)))
	assert.Equal(t, 1445, fs2(strings.NewReader(`5 + (8 * 3 + 9 + 3 * 4 * 3)`)))
	assert.Equal(t, 669060, fs2(strings.NewReader(`5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))`)))
	assert.Equal(t, 23340, fs2(strings.NewReader(`((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2`)))
}

func TestFs2Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 42, fs2(f))
}

func Test_append(t *testing.T) {
	assert.Equal(t, "x012", app("012", 0, 'x'))
	assert.Equal(t, "0x12", app("012", 1, 'x'))
	assert.Equal(t, "01x2", app("012", 2, 'x'))
	assert.Equal(t, "012x", app("012", 3, 'x'))
}

func Test_findPrevious(t *testing.T) {
	assert.Equal(t, 0, findPrevious("1 + 3 + 2", 0))
	assert.Equal(t, 4, findPrevious("1 + 3 + 2", 4))
	assert.Equal(t, 4, findPrevious("1 + 33 + 2", 4))
	assert.Equal(t, 4, findPrevious("1 + (1 + 2) + 2", 10))
}

func Test_findNext(t *testing.T) {
	assert.Equal(t, 5, findNext("1 + 3", 4))
	assert.Equal(t, 5, findNext("1 + 3 + 4", 4))
}

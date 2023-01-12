package main

import (
	"os"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestFs1Test(t *testing.T) {
	assert.Equal(t, 6, fs1(strings.NewReader("ADVENT")))
	assert.Equal(t, 7, fs1(strings.NewReader("A(1x5)BC")))
	assert.Equal(t, 9, fs1(strings.NewReader("(3x3)XYZ")))
	assert.Equal(t, 11, fs1(strings.NewReader("A(2x2)BCD(2x2)EFG")))
	assert.Equal(t, 6, fs1(strings.NewReader("(6x1)(1x3)A")))
	assert.Equal(t, 18, fs1(strings.NewReader("X(8x2)(3x3)ABCY")))
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	v := fs1(f)
	assert.Equal(t, 123908, v)
}

func TestFs2Test(t *testing.T) {
	assert.Equal(t, 9, fs2(strings.NewReader("(3x3)XYZ")))
	assert.Equal(t, 20, fs2(strings.NewReader("X(8x2)(3x3)ABCY")))
	assert.Equal(t, 241920, fs2(strings.NewReader("(27x12)(20x12)(13x14)(7x10)(1x12)A")))
	assert.Equal(t, 445, fs2(strings.NewReader("(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN")))
}

func TestFs2Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	v := fs2(f)
	assert.Equal(t, 10755693147, v)
}

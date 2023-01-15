package main

import (
	"fmt"
	"os"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestRotate(t *testing.T) {
	testRotate(t, "abcd", "dabc", 1)
	testRotate(t, "abcd", "bcda", -1)
	testRotate(t, "abcd", "cdab", 2)
	testRotate(t, "abcd", "abcd", 4)
	testRotate(t, "abcd", "cdab", 6)
	testRotate(t, "abcd", "cdab", -2)
}

func testRotate(t *testing.T, in string, expected string, i int) {
	runes := []rune(in)
	rotate(runes, i)
	assert.Equal(t, expected, string(runes))
}

func TestApply(t *testing.T) {
	testMove(t, "bcdea", "bdeac", 1, 4)
	testMove(t, "bdeac", "abdec", 3, 0)
	testMove(t, "bcdea", "bcdea", 1, 1)
	testMove(t, "abcde", "bacde", 0, 1)
	testMove(t, "abcde", "bcdea", 0, 4)
	testMove(t, "abcde", "abced", 4, 3)
	testMove(t, "abcde", "eabcd", 4, 0)
}

func testMove(t *testing.T, in, expected string, x, y int) {
	runes := []rune(in)
	Move{x, y}.apply(runes)
	assert.Equal(t, expected, string(runes))
}

func TestFs1Test(t *testing.T) {
	f, err := os.Open("test.txt")
	require.NoError(t, err)
	assert.Equal(t, "decab", fs1(f, "abcde"))
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, "fdhbcgea", fs1(f, "abcdefgh"))
}

func TestFs2Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, "egfbcadh", fs2(f, "fbgdceah"))
}

func Test_permutations(t *testing.T) {
	fmt.Printf("%v\n", permutations(0, []rune("abc")))
}

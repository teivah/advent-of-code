package main

import (
	"os"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestParse(t *testing.T) {
	v := "[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]"
	pair, _ := parse(v, 1)
	assert.Equal(t, v, pair.String())
}

func TestFs1Test(t *testing.T) {
	f, err := os.Open("test.txt")
	require.NoError(t, err)
	assert.Equal(t, 42, fs1(f))
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 42, fs1(f))
}

func TestFs2Test(t *testing.T) {
	f, err := os.Open("test.txt")
	require.NoError(t, err)
	assert.Equal(t, 42, fs2(f))
}

func TestFs2Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 42, fs2(f))
}

func TestExplode(t *testing.T) {
	assert.Equal(t, "[[[[0,9],2],3],4]", explode("[[[[[9,8],1],2],3],4]"))
	assert.Equal(t, "[7,[6,[5,[7,0]]]]", explode("[7,[6,[5,[4,[3,2]]]]]"))
	assert.Equal(t, "[[6,[5,[7,0]]],3]", explode("[[6,[5,[4,[3,2]]]],1]"))
	assert.Equal(t, "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]", explode("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]"))
	assert.Equal(t, "[[3,[2,[8,0]]],[9,[5,[7,0]]]]", explode("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"))
}

func Test_transformString(t *testing.T) {
	assert.Equal(t, "01x45", transformString("012345", 2, 4, "x"))
	assert.Equal(t, "01x345", transformString("012345", 2, 3, "x"))
}

func Test_add(t *testing.T) {
	add()
}

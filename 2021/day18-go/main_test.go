package main

import (
	"os"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestFs1Test(t *testing.T) {
	f, err := os.Open("test.txt")
	require.NoError(t, err)
	assert.Equal(t, "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]", fs1(f))
}

func TestFs1Unit(t *testing.T) {
	assert.Equal(t, "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]", fs1(strings.NewReader(`[[[[4,3],4],4],[7,[[8,4],9]]]
[1,1]`)))
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 4072, magnitude(fs1(f)))
}

func TestFs2Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 4483, fs2(f))
}

func TestExplode(t *testing.T) {
	testExplode(t, "[[[[[9,8],1],2],3],4]", "[[[[0,9],2],3],4]")
	testExplode(t, "[7,[6,[5,[4,[3,2]]]]]", "[7,[6,[5,[7,0]]]]")
	testExplode(t, "[[6,[5,[4,[3,2]]]],1]", "[[6,[5,[7,0]]],3]")
	testExplode(t, "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]", "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")
	testExplode(t, "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]", "[[3,[2,[8,0]]],[9,[5,[7,0]]]]")
	testExplode(t, "[[[[0,7],4],[7,[[8,4],9]]],[1,1]]", "[[[[0,7],4],[15,[0,13]]],[1,1]]")
	testExplode(t, "[[[[0,7],4],[900,[[200,200],9900]]],[1,1]]", "[[[[0,7],4],[1100,[0,10100]]],[1,1]]")
	testExplode(t, "[[[[4,0],[5,4]],[[7,0],[[7,8],5]]],[10,[[11,9],[11,0]]]]",
		"[[[[4,0],[5,4]],[[7,7],[0,13]]],[10,[[11,9],[11,0]]]]")
}

func testExplode(t *testing.T, from, to string) {
	s, b := explode(from)
	assert.True(t, b)
	assert.Equal(t, to, s)
}

func TestTransformString(t *testing.T) {
	assert.Equal(t, "01x45", transformString("012345", 2, 4, "x"))
	assert.Equal(t, "01x345", transformString("012345", 2, 3, "x"))
}

func TestAdd(t *testing.T) {
	assert.Equal(t, "[[1,2],[[3,4],5]]", add("[1,2]", "[[3,4],5]"))
}

func TestSplit(t *testing.T) {
	s, b := split("[[[[0,7],4],[15,[0,13]]],[1,1]]")
	assert.True(t, b)
	assert.Equal(t, "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]", s)

	s2, b2 := split("[[[[0,7],4],[[7,8],[0,13]]],[1,1]]")
	assert.True(t, b2)
	assert.Equal(t, "[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]", s2)

	s2, b2 = split("[9,[19,9]]")
	assert.True(t, b2)
	assert.Equal(t, "[9,[[9,10],9]]", s2)

	s2, b2 = split("[9,[19,9]]")
	assert.True(t, b2)
	assert.Equal(t, "[9,[[9,10],9]]", s2)
}

func TestMagnitude(t *testing.T) {
	assert.Equal(t, 129, magnitude("[[9,1],[1,9]]"))
	assert.Equal(t, 3488, magnitude("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"))
}

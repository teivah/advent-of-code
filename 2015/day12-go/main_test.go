package main

import (
	"os"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestFs1Test(t *testing.T) {
	test1(t, "[1,2,3]", 6)
	test1(t, `{"a":2,"b":4}`, 6)
	test1(t, `[[[3]]]`, 3)
	test1(t, `{"a":{"b":4},"c":-1}`, 3)
	test1(t, `{"a":[-1,1]}`, 0)
	test1(t, `[-1,{"a":1}]`, 0)
	test1(t, `[]`, 0)
	test1(t, `{}`, 0)
}

func test1(t *testing.T, s string, expected int) {
	v, err := fs1(strings.NewReader(s))
	require.NoError(t, err)
	assert.Equal(t, expected, v)
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	v, err := fs1(f)
	require.NoError(t, err)
	assert.Equal(t, 119433, v)
}

func TestFs2Test(t *testing.T) {
	test2(t, "[1,2,3]", 6)
	test2(t, `[1,{"c":"red","b":2},3]`, 4)
	test2(t, `{"d":"red","e":[1,2,3,4],"f":5}`, 0)
	test2(t, `[1,"red",5]`, 6)
	test2(t, `[[1]]`, 1)
}

func test2(t *testing.T, s string, expected int) {
	v, err := fs2(strings.NewReader(s))
	require.NoError(t, err)
	assert.Equal(t, expected, v)
}

func TestFs2Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	v, err := fs2(f)
	require.NoError(t, err)
	assert.Equal(t, 42, v)
}

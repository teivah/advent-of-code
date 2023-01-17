package main

import (
	"fmt"
	"os"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestFs1Test(t *testing.T) {
	f, err := os.Open("test.txt")
	require.NoError(t, err)
	v, err := fs1(f)
	require.NoError(t, err)
	assert.Equal(t, 14, v)
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	v, err := fs1(f)
	require.NoError(t, err)
	assert.Equal(t, 428, v)
}

func TestFs2Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	v, err := fs2(f)
	require.NoError(t, err)
	assert.Equal(t, 42, v)
}

func Test_getAllPairs(t *testing.T) {
	fmt.Println(getAllPairs(3))
}

func Test_permutations(t *testing.T) {
	fmt.Println(permutations(1, []int{1, 2, 3, 4}))
}

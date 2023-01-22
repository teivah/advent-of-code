package main

import (
	"fmt"
	"os"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestFs1Test(t *testing.T) {
	f, err := os.Open("test.txt")
	require.NoError(t, err)
	assert.Equal(t, 10, fs1(f))
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 10804, fs1(f))
}

func TestFs2Test(t *testing.T) {
	assert.Equal(t, 4, fs2(strings.NewReader(`dabAcCaCBAcCcaDA`)))
}

func TestFs2Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 6650, fs2(f))
}

func Test_isOpposite(t *testing.T) {
	fmt.Println(isOpposite('R', 'r'))
}

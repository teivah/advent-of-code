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
	assert.Equal(t, "7,3", fs1(f))
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, "32,99", fs1(f))
}

func TestFs2Test(t *testing.T) {
	assert.Equal(t, "6,4", fs2(strings.NewReader(`/>-<\  
|   |  
| /<+-\
| | | v
\>+</ |
  |   ^
  \<->/`)))
}

func TestFs2Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, "56,31", fs2(f))
}

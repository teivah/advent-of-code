package main

import (
	"os"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestFs1Unit(t *testing.T) {
	assert.Equal(t, 2, fs1(strings.NewReader(`000122000011`), 2, 3))
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 2250, fs1(f, 6, 25))
}

func TestFs2Test(t *testing.T) {
	fs2(strings.NewReader(`0222112222120000`), 2, 2)
}

func TestFs2Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	fs2(f, 6, 25)
}

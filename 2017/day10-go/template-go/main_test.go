package main

import (
	"os"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestFs1Test(t *testing.T) {
	assert.Equal(t, 2, fs1(strings.NewReader(`ne,ne,s`)))
	assert.Equal(t, 3, fs1(strings.NewReader(`ne,ne,ne`)))
	assert.Equal(t, 0, fs1(strings.NewReader(`ne,ne,sw,sw`)))
	assert.Equal(t, 2, fs1(strings.NewReader(`ne,ne,s,s`)))
	assert.Equal(t, 3, fs1(strings.NewReader(`se,sw,se,sw,sw`)))
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 705, fs1(f))
}

func TestFs2Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 42, fs2(f))
}

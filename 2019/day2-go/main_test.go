package main

import (
	"os"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestFs1Test(t *testing.T) {
	assert.Equal(t, 3500, fs1(strings.NewReader(`1,9,10,3,2,3,11,0,99,30,40,50`), nil))
	assert.Equal(t, 2, fs1(strings.NewReader(`1,0,0,0,99`), nil))
	assert.Equal(t, 30, fs1(strings.NewReader(`1,1,1,4,99,5,6,0,99`), nil))
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 5110675, fs1(f, map[int]int{1: 12, 2: 2}))
}

func TestFs2Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 4847, fs2(f))
}

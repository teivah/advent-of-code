package main

import (
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestRandom(t *testing.T) {
	assert.Equal(t, "100", random("1"))
	assert.Equal(t, "001", random("0"))
	assert.Equal(t, "11111000000", random("11111"))
	assert.Equal(t, "1111000010100101011110000", random("111100001010"))
}

func TestChecksum(t *testing.T) {
	assert.Equal(t, "100", checksum("110010110100"))
}

func TestFs1Input(t *testing.T) {
	v, err := fs("01111010110010011", 272)
	require.NoError(t, err)
	assert.Equal(t, "00100111000101111", v)
}

func TestFs2Input(t *testing.T) {
	v, err := fs("01111010110010011", 35651584)
	require.NoError(t, err)
	assert.Equal(t, "11101110011100110", v)
}

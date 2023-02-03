package main

import (
	"os"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func Test_seatID(t *testing.T) {
	assert.Equal(t, 357, seatID(position("FBFBBFFRLR")))
	assert.Equal(t, 567, seatID(position("BFFFBBFRRR")))
	assert.Equal(t, 119, seatID(position("FFFBBBFRRR")))
	assert.Equal(t, 820, seatID(position("BBFFBBFRLL")))
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 915, fs1(f))
}

func TestFs2Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 42, fs2(f))
}

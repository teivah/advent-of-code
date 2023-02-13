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
	assert.Equal(t, 590784, fs1(f))
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 581108, fs1(f))
}

func TestFs2Unit(t *testing.T) {
	assert.Equal(t, 1000, fs2(strings.NewReader(`on x=0..9,y=0..9,z=0..9
	on x=5..5,y=5..5,z=5..5`)))
	//1
	assert.Equal(t, 876, fs2(strings.NewReader(`on x=0..9,y=0..9,z=0..9
	off x=4..8,y=4..8,z=4..8
	on x=5..5,y=5..5,z=5..5`)))
	// 2
	assert.Equal(t, 1000, fs2(strings.NewReader(`on x=0..9,y=0..9,z=0..9
	off x=5..5,y=5..5,z=5..5
	on x=4..8,y=4..8,z=4..8`)))
	// 3
	// 4
	assert.Equal(t, 1000, fs2(strings.NewReader(`on x=0..9,y=0..9,z=0..9
	on x=4..8,y=4..8,z=4..8
	on x=5..5,y=5..5,z=5..5`)))
	// 5
	assert.Equal(t, 1000, fs2(strings.NewReader(`on x=0..9,y=0..9,z=0..9
	on x=5..5,y=5..5,z=5..5
	on x=4..8,y=4..8,z=4..8`)))
	// 6
	assert.Equal(t, 1000, fs2(strings.NewReader(`on x=0..9,y=0..9,z=0..9
	on x=5..5,y=5..5,z=5..5
	on x=6..6,y=6..6,z=6..6`)))
	// 7
	assert.Equal(t, 250, fs2(strings.NewReader(`on x=0..4,y=0..4,z=0..4
	on x=5..9,y=5..9,z=5..9`)))
	// 8
	assert.Equal(t, 223, fs2(strings.NewReader(`on x=0..4,y=0..4,z=0..4
	on x=2..6,y=2..6,z=2..6`)))
	// 9
	assert.Equal(t, 875, fs2(strings.NewReader(`on x=0..9,y=0..9,z=0..9
	off x=4..8,y=4..8,z=4..8
	off x=5..5,y=5..5,z=5..5`)))
	// 10
	assert.Equal(t, 875, fs2(strings.NewReader(`on x=0..9,y=0..9,z=0..9
	off x=5..5,y=5..5,z=5..5
	off x=4..8,y=4..8,z=4..8`)))
	// 11
	// 12
	assert.Equal(t, 874, fs2(strings.NewReader(`on x=0..9,y=0..9,z=0..9
	off x=4..8,y=4..8,z=4..8
	off x=9..9,y=9..9,z=9..9`)))
	// 13
	assert.Equal(t, 1000, fs2(strings.NewReader(`on x=0..9,y=0..9,z=0..9
	off x=5..5,y=5..5,z=5..5
	on x=4..8,y=4..8,z=4..8`)))
	// 14
	// 15
	assert.Equal(t, 999, fs2(strings.NewReader(`on x=0..9,y=0..9,z=0..9
	on x=4..8,y=4..8,z=4..8
	off x=5..5,y=5..5,z=5..5`)))
	//16
	assert.Equal(t, 876, fs2(strings.NewReader(`on x=0..9,y=0..9,z=0..9
	on x=5..5,y=5..5,z=5..5
	off x=4..8,y=4..8,z=4..8
	on x=5..5,y=5..5,z=5..5`)))
	// 17
	assert.Equal(t, 875, fs2(strings.NewReader(`on x=0..9,y=0..9,z=0..9
	on x=5..5,y=5..5,z=5..5
	off x=4..8,y=4..8,z=4..8`)))
	// 21
	assert.Equal(t, 1000, fs2(strings.NewReader(`on x=0..9,y=0..9,z=0..9
	off x=5..5,y=5..5,z=5..5
	off x=5..5,y=5..5,z=5..5
	on x=5..5,y=5..5,z=5..5`)))
}

func TestFs2Test(t *testing.T) {
	f, err := os.Open("test2.txt")
	require.NoError(t, err)
	assert.Equal(t, 2758514936282235, fs2(f))
}

func TestFs2Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 1325473814582641, fs2(f))
}

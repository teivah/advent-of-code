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
	assert.Equal(t, 590784, fs1(f))
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 42, fs1(f))
}

func TestFs2Test(t *testing.T) {
	f, err := os.Open("test2.txt")
	require.NoError(t, err)
	assert.Equal(t, 2758514936282235, fs2(f))
}

func TestFs2Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 42, fs2(f))
}

func TestCube_overlap(t *testing.T) {
	c := newCube(Position{}, Position{9, 9, 9})
	c.on(&Cube{from: Position{5, 5, 5}, to: Position{5, 5, 5}})
	fmt.Println(c.count)
}

func Test_newCube(t *testing.T) {
	c := newCube(Position{}, Position{9, 9, 9})
	c.on(newCube(Position{5, 5, 5}, Position{5, 5, 5}))
}

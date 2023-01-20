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
	assert.Equal(t, 31, fs1(f))
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 42, fs1(f))
}

func TestFs2Test(t *testing.T) {
	f, err := os.Open("test.txt")
	require.NoError(t, err)
	assert.Equal(t, 19, fs2(f))
}

func TestFs2Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 1841, fs2(f))
}

func TestPorts_remove(t *testing.T) {
	p := &Ports{map[int][]int{1: {1, 2, 3}}}
	a := p.remove(1, 1)
	fmt.Println(p.ports)
	b := p.remove(1, 2)
	fmt.Println(p.ports)
	c := p.remove(1, 3)
	fmt.Println(p.ports)
	c()
	b()
	a()
	fmt.Println(p.ports)
}

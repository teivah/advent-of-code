package main

import (
	"os"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestFs1Test(t *testing.T) {
	//assert.Equal(t, 31, fs1(strings.NewReader(`10 ORE => 10 A
	//1 ORE => 1 B
	//7 A, 1 B => 1 C
	//7 A, 1 C => 1 D
	//7 A, 1 D => 1 E
	//7 A, 1 E => 1 FUEL`)))

	assert.Equal(t, 165, fs1(strings.NewReader(`9 ORE => 2 A
8 ORE => 3 B
7 ORE => 5 C
3 A, 4 B => 1 AB
5 B, 7 C => 1 BC
4 C, 1 A => 1 CA
2 AB, 3 BC, 4 CA => 1 FUEL`)))

	//	assert.Equal(t, 10, fs1(strings.NewReader(`10 ORE => 10 A
	//8 A => 1 B
	//2 A, 1 B => 1 FUEL`)))

	//assert.Equal(t, 10, fs1(strings.NewReader(`10 ORE => 1 FUEL`)))
	//assert.Equal(t, 100, fs1(strings.NewReader(`10 ORE => 10 A
	//100 A => 1 FUEL`)))
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 42, fs1(f))
}

func TestFs2Test(t *testing.T) {
	f, err := os.Open("test.txt")
	require.NoError(t, err)
	assert.Equal(t, 42, fs2(f))
}

func TestFs2Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 42, fs2(f))
}

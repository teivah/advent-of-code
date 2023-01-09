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
	v, err := fs1(f)
	require.NoError(t, err)
	assert.Equal(t, 4, v)
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	v, err := fs1(f)
	require.NoError(t, err)
	assert.Equal(t, 576, v)
}

func TestFs2Test(t *testing.T) {
	v, err := fs2(
		strings.NewReader(
			`e => H
e => O
H => HO
H => OH
O => HH

HOH`,
		),
	)
	require.NoError(t, err)
	assert.Equal(t, 3, v)

	v, err = fs2(
		strings.NewReader(
			`e => H
e => O
H => HO
H => OH
O => HH

HOHOHO`,
		),
	)
	require.NoError(t, err)
	assert.Equal(t, 6, v)
}

func TestFs2Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	v, err := fs2(f)
	require.NoError(t, err)
	assert.Equal(t, 207, v)
}

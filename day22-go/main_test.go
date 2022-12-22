package main

import (
	"os"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestFs1Test(t *testing.T) {
	f, err := os.Open("test.txt")
	require.NoError(t, err)
	v, err := fs1(f)
	require.NoError(t, err)
	assert.Equal(t, 6032, v)
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	v, err := fs1(f)
	require.NoError(t, err)
	assert.Equal(t, 42, v)
}

func TestFs2Test(t *testing.T) {
	f, err := os.Open("test.txt")
	require.NoError(t, err)
	v, err := fs2(f, 4, map[int]FaceHint{
		1: {
			left: Hint{
				face:      3,
				direction: Up,
				opposite:  false,
			},
			right: Hint{
				face:      6,
				direction: Right,
				opposite:  true,
			},
			up: Hint{
				face:      2,
				direction: Up,
				opposite:  true,
			},
			down: Hint{
				face:      4,
				direction: Up,
				opposite:  false,
			},
		},
		2: {
			left: Hint{
				face:      6,
				direction: Down,
				opposite:  true,
			},
			right: Hint{
				face:      3,
				direction: Left,
				opposite:  false,
			},
			up: Hint{
				face:      1,
				direction: Up,
				opposite:  true,
			},
			down: Hint{
				face:      5,
				direction: Down,
				opposite:  true,
			},
		},
		3: {
			left: Hint{
				face:      2,
				direction: Right,
				opposite:  false,
			},
			right: Hint{
				face:      4,
				direction: Left,
				opposite:  false,
			},
			up: Hint{
				face:      1,
				direction: Left,
				opposite:  false,
			},
			down: Hint{
				face:      5,
				direction: Left,
				opposite:  true,
			},
		},
		4: {
			left: Hint{
				face:      3,
				direction: Right,
				opposite:  false,
			},
			right: Hint{
				face:      6,
				direction: Up,
				opposite:  true,
			},
			up: Hint{
				face:      1,
				direction: Down,
				opposite:  false,
			},
			down: Hint{
				face:      5,
				direction: Up,
				opposite:  false,
			},
		},
		5: {
			left: Hint{
				face:      3,
				direction: Down,
				opposite:  true,
			},
			right: Hint{
				face:      6,
				direction: Left,
				opposite:  false,
			},
			up: Hint{
				face:      4,
				direction: Down,
				opposite:  false,
			},
			down: Hint{
				face:      2,
				direction: Down,
				opposite:  true,
			},
		},
		6: {
			left: Hint{
				face:      5,
				direction: Right,
				opposite:  false,
			},
			right: Hint{
				face:      1,
				direction: Right,
				opposite:  true,
			},
			up: Hint{
				face:      4,
				direction: Right,
				opposite:  true,
			},
			down: Hint{
				face:      2,
				direction: Left,
				opposite:  true,
			},
		},
	})
	require.NoError(t, err)
	assert.Equal(t, 42, v)
}

func TestFs2Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	v, err := fs2(f, 50, nil)
	require.NoError(t, err)
	assert.Equal(t, 42, v)
}

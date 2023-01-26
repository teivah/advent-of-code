package main

import (
	"os"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	lib "github.com/teivah/advent-of-code"
)

func TestFs1Test(t *testing.T) {
	f, err := os.Open("test.txt")
	require.NoError(t, err)
	v, _ := fs1(f, 0)
	assert.Equal(t, 5216, v)
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	v, _ := fs1(f, 0)
	assert.Equal(t, 19974, v)
}

func TestFs2Test(t *testing.T) {
	f, err := os.Open("test.txt")
	require.NoError(t, err)
	v, _ := fs1(f, 1570)
	assert.Equal(t, 51, v)
}

func TestFs2Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 6301, fs2(f))
}

func Test_parse(t *testing.T) {
	f, err := os.Open("test.txt")
	require.NoError(t, err)
	lines := lib.ReaderToStrings(f)
	g1, g2 := parse(lines, 0)
	assert.Equal(t, map[string]Group{
		"immune group 1": {
			id:         "immune group 1",
			immuneArmy: true,
			units:      17,
			hitPoints:  5390,
			weakTo: map[string]struct{}{
				"radiation":   {},
				"bludgeoning": {},
			},
			immuneTo:   map[string]struct{}{},
			damage:     4507,
			attackType: "fire",
			initiative: 2,
		},
		"immune group 2": {
			id:         "immune group 2",
			immuneArmy: true,
			units:      989,
			hitPoints:  1274,
			immuneTo: map[string]struct{}{
				"fire": {},
			},
			weakTo: map[string]struct{}{
				"bludgeoning": {},
				"slashing":    {},
			},
			damage:     25,
			attackType: "slashing",
			initiative: 3,
		},
	}, g1)
	assert.Equal(t, map[string]Group{
		"infection group 1": {
			id:        "infection group 1",
			units:     801,
			hitPoints: 4706,
			weakTo: map[string]struct{}{
				"radiation": {},
			},
			immuneTo:   map[string]struct{}{},
			damage:     116,
			attackType: "bludgeoning",
			initiative: 1,
		},
		"infection group 2": {
			id:        "infection group 2",
			units:     4485,
			hitPoints: 2961,
			immuneTo: map[string]struct{}{
				"radiation": {},
			},
			weakTo: map[string]struct{}{
				"fire": {},
				"cold": {},
			},
			damage:     12,
			attackType: "slashing",
			initiative: 4,
		},
	}, g2)
}

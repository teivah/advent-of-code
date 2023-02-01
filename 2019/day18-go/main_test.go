package main

import (
	"os"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestFs1Test(t *testing.T) {
	assert.Equal(t, 8, fs1(strings.NewReader(`#########
	#b.A.@.a#
	#########`)))

	assert.Equal(t, 86, fs1(strings.NewReader(`########################
	#f.D.E.e.C.b.A.@.a.B.c.#
	######################.#
	#d.....................#
	########################`)))

	assert.Equal(t, 132, fs1(strings.NewReader(`########################
	#...............b.C.D.f#
	#.######################
	#.....@.a.B.c.d.A.e.F.g#
	########################`)))

	assert.Equal(t, 136, fs1(strings.NewReader(`#################
	#i.G..c...e..H.p#
	########.########
	#j.A..b...f..D.o#
	########@########
	#k.E..a...g..B.n#
	########.########
	#l.F..d...h..C.m#
	#################`)))

	assert.Equal(t, 81, fs1(strings.NewReader(`########################
	#@..............ac.GI.b#
	###d#e#f################
	###A#B#C################
	###g#h#i################
	########################`)))
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 5102, fs1(f))
}

func TestFs2Test(t *testing.T) {
	assert.Equal(t, 8, fs2(strings.NewReader(
		`#######
	#a.#Cd#
	##@#@##
	#######
	##@#@##
	#cB#Ab#
	#######`)))

	assert.Equal(t, 24, fs2(strings.NewReader(
		`###############
	#d.ABC.#.....a#
	######@#@######
	###############
	######@#@######
	#b.....#.....c#
	###############`)))

	assert.Equal(t, 32, fs2(strings.NewReader(
		`#############
	#DcBa.#.GhKl#
	#.###@#@#I###
	#e#d#####j#k#
	###C#@#@###J#
	#fEbA.#.FgHi#
	#############`)))

	assert.Equal(t, 72, fs2(strings.NewReader(`#############
	#g#f.D#..h#l#
	#F###e#E###.#
	#dCba@#@BcIJ#
	#############
	#nK.L@#@G...#
	#M###N#H###.#
	#o#m..#i#jk.#
	#############`)))
}

func TestFs2Input(t *testing.T) {
	f, err := os.Open("input2.txt")
	require.NoError(t, err)
	assert.Equal(t, 2348, fs2(f))
}

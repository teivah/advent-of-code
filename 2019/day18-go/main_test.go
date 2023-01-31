package main

import (
	"os"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestFs1Test(t *testing.T) {
	//	assert.Equal(t, 8, fs1(strings.NewReader(`#########
	//#b.A.@.a#
	//#########`)))

	//assert.Equal(t, 86, fs1(strings.NewReader(`########################
	//#f.D.E.e.C.b.A.@.a.B.c.#
	//######################.#
	//#d.....................#
	//########################`)))
	//
	//assert.Equal(t, 132, fs1(strings.NewReader(`########################
	//#...............b.C.D.f#
	//#.######################
	//#.....@.a.B.c.d.A.e.F.g#
	//########################`)))

	//assert.Equal(t, 136, fs1(strings.NewReader(`#################
	//#i.G..c...e..H.p#
	//########.########
	//#j.A..b...f..D.o#
	//########@########
	//#k.E..a...g..B.n#
	//########.########
	//#l.F..d...h..C.m#
	//#################`)))
	//
	//assert.Equal(t, 81, fs1(strings.NewReader(`########################
	//	#@..............ac.GI.b#
	//	###d#e#f################
	//	###A#B#C################
	//	###g#h#i################
	//	########################`)))
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

package main

import (
	"fmt"
	"os"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	lib "github.com/teivah/advent-of-code"
)

func TestFs1Unit(t *testing.T) {
	assert.Equal(t, 8, fs1(strings.NewReader(`.#..#
	.....
	#####
	....#
	...##`)))

	assert.Equal(t, 33, fs1(strings.NewReader(`......#.#.
	#..#.#....
	..#######.
	.#.#.###..
	.#..#.....
	..#....#.#
	#..#....#.
	.##.#..###
	##...#..#.
	.#....####`)))

	assert.Equal(t, 210, fs1(strings.NewReader(`.#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##`)))
}

func TestFs1Test(t *testing.T) {
	f, err := os.Open("test.txt")
	require.NoError(t, err)
	assert.Equal(t, 2, fs1(f))
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 42, fs1(f))
}

func TestFs2Test(t *testing.T) {
	assert.Equal(t, 1501, fs2(strings.NewReader(`.#....#####...#..
	##...##.#####..##
	##...#...#.#####.
	..#.....X...###..
	..#.#.....#....##`), 9))

	//assert.Equal(t, 802, fs2(strings.NewReader(`.#..##.###...#######
	//##.############..##.
	//.#.######.########.#
	//.###.#######.####.#.
	//#####.##.#.##.###.##
	//..#####..#.#########
	//####################
	//#.####....###.#.#.##
	//##.#################
	//#####.##.###..####..
	//..######..##.#######
	//####.##.####...##..#
	//.#####..#.######.###
	//##...#.##########...
	//#.##########.#######
	//.####.#.###.###.#.##
	//....##.##.###..#####
	//.#.#.###########.###
	//#.#.#.#####.####.###
	//###.##.####.##.#..##`), 200))
}

func TestFs2Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 42, fs2(f, 0))
}

func Test_canDetect(t *testing.T) {
	assert.True(t, canDetect([][]bool{
		{true, false, false},
		{false, false, false},
		{false, false, false},
	}, lib.Position{2, 2}, lib.Position{0, 0}))

	assert.False(t, canDetect([][]bool{
		{true, false, false},
		{false, true, false},
		{false, false, false},
	}, lib.Position{2, 2}, lib.Position{0, 0}))

	assert.False(t, canDetect([][]bool{
		{false, true, false, false, false, false},
		{false, false, false, true, false, false},
		{false, false, false, false, false, true},
	}, lib.Position{2, 5}, lib.Position{0, 1}))
}

func Test_isZero(t *testing.T) {
	fmt.Printf("%v\n", isInt(3.999999999999999))
}

func Test_angle(t *testing.T) {
	assert.Equal(t, float64(0), calcAngle(lib.Position{0, 0}, lib.Position{0, 1}))
	assert.Equal(t, float64(90), calcAngle(lib.Position{0, 0}, lib.Position{-1, 0}))
	assert.Equal(t, float64(135), calcAngle(lib.Position{0, 0}, lib.Position{-1, -1}))
	assert.Equal(t, float64(180), calcAngle(lib.Position{0, 0}, lib.Position{0, -1}))
	assert.Equal(t, float64(270), calcAngle(lib.Position{0, 0}, lib.Position{1, 0}))
}

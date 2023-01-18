package main

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestFs1Test(t *testing.T) {
	assert.Equal(t, 12, fs1(5, []int{3, 4, 1, 5}))
}

func TestFs1Input(t *testing.T) {
	assert.Equal(t, 12, fs1(256, []int{76, 1, 88, 148, 166, 217, 130, 0, 128, 254, 16, 2, 130, 71, 255, 229}))
}

func TestFs2Test(t *testing.T) {
	assert.Equal(t, "a2582a3a0e66e6e86e3812dcb672a272", fs2(256, ""))
	assert.Equal(t, "33efeb34ea91902bb2f59c9920caa6cd", fs2(256, "AoC 2017"))
	assert.Equal(t, "3efbe78a8d82f29979031a4aa0b16a9d", fs2(256, "1,2,3"))
	assert.Equal(t, "63960835bcdc130f0b66d7ff4f6a5a8e", fs2(256, "1,2,4"))
}

func TestFs2Input(t *testing.T) {
	assert.Equal(t, 42, fs2(256, "76,1,88,148,166,217,130,0,128,254,16,2,130,71,255,229"))
}

func Test_reverse(t *testing.T) {
	testReverse(t, []int{0, 1, 2, 3, 4, 5}, 1, 3, []int{0, 3, 2, 1, 4, 5})
	testReverse(t, []int{0, 1, 2, 3, 4, 5}, 1, 4, []int{0, 4, 3, 2, 1, 5})
	testReverse(t, []int{0, 1, 2, 3, 4, 5}, 4, 2, []int{0, 1, 2, 3, 5, 4})
	testReverse(t, []int{0, 1, 2, 3, 4, 5}, 4, 3, []int{4, 1, 2, 3, 0, 5})
	testReverse(t, []int{0, 1, 2, 3, 4, 5}, 4, 4, []int{5, 4, 2, 3, 1, 0})
}

func testReverse(t *testing.T, in []int, i int, length int, expected []int) {
	reverse(in, i, length)
	assert.Equal(t, expected, in)
}

func Test_toSequence(t *testing.T) {
	assert.Equal(t, []int{49, 44, 50, 44, 51, 17, 31, 73, 47, 23}, toSequence("1,2,3"))
}

func Test_knotHash(t *testing.T) {
	assert.Equal(t, "4007ff", knotHash([]int{64, 7, 255}))
}

func Test_denseHash(t *testing.T) {
	assert.Equal(t, []int{64}, denseHash([]int{65, 27, 9, 1, 4, 3, 40, 50, 91, 7, 6, 0, 2, 5, 68, 22}))
}

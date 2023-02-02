package main

import (
	"fmt"
	"os"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestFs1Test(t *testing.T) {
	assert.Equal(t, 0, fs1(strings.NewReader(`deal into new stack
cut -2
deal with increment 7
cut 8
cut -4
deal with increment 7
cut 3
deal with increment 9
deal with increment 3
cut -1`), 10, 9))
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 3074, fs1(f, 10007, 2019))
}

func TestFs2Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, int64(104073967000066), fs2(f, 119315717514047, 101741582076661, 2020))
}

func Test_deal(t *testing.T) {
	assert.Equal(t, []int{9, 8, 7, 6, 5, 4, 3, 2, 1, 0}, deal()([]int{0, 1, 2, 3, 4, 5, 6, 7, 8, 9}))
}

func Test_increment(t *testing.T) {
	assert.Equal(t, []int{0, 7, 4, 1, 8, 5, 2, 9, 6, 3}, increment(3)([]int{0, 1, 2, 3, 4, 5, 6, 7, 8, 9}))
}

func Test_cut(t *testing.T) {
	assert.Equal(t, []int{3, 4, 5, 6, 7, 8, 9, 0, 1, 2}, cut(3)([]int{0, 1, 2, 3, 4, 5, 6, 7, 8, 9}))
	assert.Equal(t, []int{6, 7, 8, 9, 0, 1, 2, 3, 4, 5}, cut(-4)([]int{0, 1, 2, 3, 4, 5, 6, 7, 8, 9}))
}

func TestFoo(t *testing.T) {
	cards := increment(3)([]int{0, 1, 2, 3, 4, 5, 6, 7, 8, 9})
	cards = increment(7)(cards)
	fmt.Println(cards)

	//cards = increment(5)([]int{0, 1, 2, 3, 4, 5, 6, 7, 8, 9})
	//fmt.Println(cards)
}

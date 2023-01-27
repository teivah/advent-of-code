package main

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestFs1Input(t *testing.T) {
	assert.Equal(t, 1653, fs1(206938, 679128))
}

func TestFs2Input(t *testing.T) {
	assert.Equal(t, 1133, fs2(206938, 679128))
}

func Test_toDigits(t *testing.T) {
	assert.Equal(t, []int{1, 2, 3, 4}, toDigits(1234))
}

func Test_twoExactDigits(t *testing.T) {
	assert.True(t, twoExactDigits([]int{1, 1, 2, 2, 3, 3}))
	assert.False(t, twoExactDigits([]int{1, 2, 3, 4, 4, 4}))
	assert.True(t, twoExactDigits([]int{1, 1, 1, 1, 2, 2}))
}

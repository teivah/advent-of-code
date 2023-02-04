package main

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestFs1Test(t *testing.T) {
	assert.Equal(t, 436, fs(2020, []int{0, 3, 6}))
}

func TestFs1Input(t *testing.T) {
	assert.Equal(t, 42, fs(2020, []int{12, 1, 16, 3, 11, 0}))
}

func TestFs2Test(t *testing.T) {
	assert.Equal(t, 175594, fs(30000000, []int{0, 3, 6}))
}

func TestFs2Input(t *testing.T) {
	assert.Equal(t, 37385, fs(30000000, []int{12, 1, 16, 3, 11, 0}))
}

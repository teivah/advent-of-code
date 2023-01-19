package main

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestFs1Test(t *testing.T) {
	assert.Equal(t, 1, fs1(4, 65, 8921))
}

func TestFs1Input(t *testing.T) {
	assert.Equal(t, 567, fs1(40_000_000, 512, 191))
}

func TestFs2Test(t *testing.T) {
	assert.Equal(t, 309, fs2(5_000_000, 65, 8921))
}

func TestFs2Input(t *testing.T) {
	assert.Equal(t, 323, fs2(5_000_000, 512, 191))
}

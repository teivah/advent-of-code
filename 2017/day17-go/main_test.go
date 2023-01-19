package main

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestFs1Test(t *testing.T) {
	assert.Equal(t, 638, fs1(3, 2017))
}

func TestFs1Input(t *testing.T) {
	assert.Equal(t, 1914, fs1(343, 2017))
}

func TestFs2Input(t *testing.T) {
	assert.Equal(t, 0, fs2(343, 50000000))
}

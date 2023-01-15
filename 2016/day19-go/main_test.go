package main

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestFs1Test(t *testing.T) {
	assert.Equal(t, 3, fs1(5))
}

func TestFs1Input(t *testing.T) {
	assert.Equal(t, 0, fs1(3017957))
}

func TestFs2Test(t *testing.T) {
	assert.Equal(t, 2, fs2(5))
}

func TestFs2Input(t *testing.T) {
	assert.Equal(t, 1423634, fs2(3017957))
}

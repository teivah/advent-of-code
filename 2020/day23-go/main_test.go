package main

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestFs1Test(t *testing.T) {
	assert.Equal(t, "92658374", fs1("389125467", 10))
	assert.Equal(t, "67384529", fs1("389125467", 100))
}

func TestFs1Input(t *testing.T) {
	assert.Equal(t, 42, fs1("963275481", 100))
}

func TestFs2Test(t *testing.T) {
	assert.Equal(t, "92658374", fs2("389125467", 10_000_000))
}

func TestFs2Input(t *testing.T) {
	assert.Equal(t, "92658374", fs2("963275481", 10_000_000))
}

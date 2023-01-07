package main

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestFs1Test(t *testing.T) {
	assert.Equal(t, 6, fs1("1", 5))
}

func TestFs1Input(t *testing.T) {
	assert.Equal(t, 329356, fs1("3113322113", 40))
}

func TestFs2Input(t *testing.T) {
	assert.Equal(t, 4666278, fs1("3113322113", 50))
}

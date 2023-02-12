package main

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestFs1Test(t *testing.T) {
	assert.Equal(t, 739785, fs1(3, 7))
}

func TestFs1Input(t *testing.T) {
	assert.Equal(t, 752745, fs1(5, 2))
}

func TestFs2Test(t *testing.T) {
	assert.Equal(t, 444356092776315, fs2(3, 7))
}

func TestFs2Input(t *testing.T) {
	assert.Equal(t, 309196008717909, fs2(5, 2))
}

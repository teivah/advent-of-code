package main

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestFs1Test(t *testing.T) {
	assert.Equal(t, 114, fs1(510, 10, 10))
}

func TestFs1Input(t *testing.T) {
	assert.Equal(t, 9659, fs1(3198, 12, 757))
}

func TestFs2Test(t *testing.T) {
	assert.Equal(t, 45, fs2(510, 10, 10))
}

func TestFs2Input(t *testing.T) {
	assert.Equal(t, 1035, fs2(3198, 12, 757))
}

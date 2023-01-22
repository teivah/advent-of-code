package main

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestFs1Test(t *testing.T) {
	assert.Equal(t, 32, fs1(9, 25))
	assert.Equal(t, 8317, fs1(10, 1618))
	assert.Equal(t, 146373, fs1(13, 7999))
	assert.Equal(t, 2764, fs1(17, 1104))
	assert.Equal(t, 54718, fs1(21, 6111))
	assert.Equal(t, 37305, fs1(30, 5807))
}

func TestFs1Input(t *testing.T) {
	assert.Equal(t, 400493, fs1(432, 71019))
}

func TestFs2Test(t *testing.T) {
	assert.Equal(t, 32, fs2(9, 25))
	assert.Equal(t, 8317, fs2(10, 1618))
	assert.Equal(t, 146373, fs2(13, 7999))
	assert.Equal(t, 2764, fs2(17, 1104))
	assert.Equal(t, 54718, fs2(21, 6111))
	assert.Equal(t, 37305, fs2(30, 5807))
}

func TestFs2Input(t *testing.T) {
	assert.Equal(t, 3338341690, fs2(432, 7101900))
}

package main

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestFs1Test(t *testing.T) {
	assert.Equal(t, 45, fs1(position{-10, 20}, position{-5, 30}))
}

func TestFs1Input(t *testing.T) {
	assert.Equal(t, 42, fs1(position{-89, 192}, position{-59, 251}))
}

func TestFs2Test(t *testing.T) {
	assert.Equal(t, 112, fs2(position{-10, 20}, position{-5, 30}))
}

func TestFs2Input(t *testing.T) {
	assert.Equal(t, 42, fs2(position{-89, 192}, position{-59, 251}))
}

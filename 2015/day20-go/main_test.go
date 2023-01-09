package main

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestFs1Input(t *testing.T) {
	v := fs1(29000000)
	assert.Equal(t, 665280, v)
}

func TestFs2Input(t *testing.T) {
	v := fs2(29000000, 50)
	assert.Equal(t, 42, v)
}

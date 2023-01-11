package main

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestFs1Input(t *testing.T) {
	v := fs1(3010, 3019)
	assert.Equal(t, "42", v)
}

package main

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestFs1Test(t *testing.T) {
	assert.Equal(t, "33,45", fs1(18))
	assert.Equal(t, "21,61", fs1(42))
}

func TestFs1Input(t *testing.T) {
	assert.Equal(t, "243,16", fs1(7857))
}

func TestFs2Test(t *testing.T) {
	assert.Equal(t, "90,269,16", fs2(18))
	assert.Equal(t, "232,251,12", fs2(42))
}

func TestFs2Input(t *testing.T) {
	assert.Equal(t, "231,227,14", fs2(7857))
}

func Test_getHundredsDigit(t *testing.T) {
	assert.Equal(t, 0, getHundredsDigit(4))
	assert.Equal(t, 3, getHundredsDigit(321))
	assert.Equal(t, 3, getHundredsDigit(4321))
}

func Test_powerLevel(t *testing.T) {
	assert.Equal(t, 4, powerLevel(3, 5, 8))
}

package main

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestFs1Test(t *testing.T) {
	assert.Equal(t, "0124515891", fs1(5))
	assert.Equal(t, "5158916779", fs1(9))
	assert.Equal(t, "9251071085", fs1(18))
	assert.Equal(t, "5941429882", fs1(2018))
}

func TestFs1Input(t *testing.T) {
	assert.Equal(t, "3656126723", fs1(880751))
}

func TestFs2Test(t *testing.T) {
	assert.Equal(t, 5, fs2("01245", 5))
	assert.Equal(t, 9, fs2("51589", 5))
	assert.Equal(t, 18, fs2("92510", 5))
	assert.Equal(t, 2018, fs2("59414", 5))
}

func TestFs2Input(t *testing.T) {
	assert.Equal(t, 20333868, fs2("880751", 6))
}

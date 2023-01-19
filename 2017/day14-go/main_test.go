package main

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestFs1Test(t *testing.T) {
	assert.Equal(t, 8108, fs1("flqrgnkx"))
}

func TestFs1Input(t *testing.T) {
	assert.Equal(t, 8250, fs1("stpzcrnm"))
}

func TestFs2Test(t *testing.T) {
	assert.Equal(t, 1242, fs2("flqrgnkx"))
}

func TestFs2Input(t *testing.T) {
	assert.Equal(t, 1113, fs2("stpzcrnm"))
}

func Test_hexToBinary(t *testing.T) {
	assert.Equal(t, "10100000110000100000000101110000", hexToBinary("a0c20170"))
}

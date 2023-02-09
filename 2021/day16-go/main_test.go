package main

import (
	"os"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestFs1Test(t *testing.T) {
	assert.Equal(t, 16, fs1(strings.NewReader(`8A004A801A8002F478`)))
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 965, fs1(f))
}

func TestFs2Input(t *testing.T) {
	assert.Equal(t, 3, fs2(strings.NewReader(`C200B40A82`)))
}

func Test_toBinary(t *testing.T) {
	assert.Equal(t, "110100101111111000101000", toBinary("D2FE28"))
}

func Test_toInt(t *testing.T) {
	assert.Equal(t, 4, toInt("100"))
}

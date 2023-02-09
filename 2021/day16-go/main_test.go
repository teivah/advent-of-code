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

func TestFs2Test(t *testing.T) {
	assert.Equal(t, 3, fs2(strings.NewReader(`C200B40A82`)))
	assert.Equal(t, 54, fs2(strings.NewReader(`04005AC33890`)))
	assert.Equal(t, 7, fs2(strings.NewReader(`880086C3E88112`)))
	assert.Equal(t, 9, fs2(strings.NewReader(`CE00C43D881120`)))
	assert.Equal(t, 1, fs2(strings.NewReader(`D8005AC2A8F0`)))
	assert.Equal(t, 0, fs2(strings.NewReader(`F600BC2D8F`)))
	assert.Equal(t, 0, fs2(strings.NewReader(`9C005AC2F8F0`)))
	assert.Equal(t, 1, fs2(strings.NewReader(`9C0141080250320F1802104A08`)))
}

func TestFs2Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 116672213160, fs2(f))
}

func Test_toBinary(t *testing.T) {
	assert.Equal(t, "110100101111111000101000", toBinary("D2FE28"))
}

func Test_toInt(t *testing.T) {
	assert.Equal(t, 4, toInt("100"))
}

package main

import (
	"os"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestFs1Test(t *testing.T) {
	f, err := os.Open("test.txt")
	require.NoError(t, err)
	assert.Equal(t, []int{109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99}, fs1(f))
	assert.Equal(t, []int{1219070632396864}, fs1(strings.NewReader(`1102,34915192,34915192,7,4,7,99,0`)))
	assert.Equal(t, []int{1125899906842624}, fs1(strings.NewReader(`104,1125899906842624,99`)))
}

func TestFs1Unit(t *testing.T) {
	//assert.Equal(t, []int{-1}, fs1(strings.NewReader(`109,-1,4,1,99`)))
	//assert.Equal(t, []int{1}, fs1(strings.NewReader(`109,-1,104,1,99`)))
	//assert.Equal(t, []int{109}, fs1(strings.NewReader(`109,-1,204,1,99`)))
	//assert.Equal(t, []int{204}, fs1(strings.NewReader(`109,1,9,2,204,-6,99`)))
	//assert.Equal(t, []int{204}, fs1(strings.NewReader(`109,1,109,9,204,-6,99`)))
	//assert.Equal(t, []int{204}, fs1(strings.NewReader(`109,1,209,-1,204,-106,99`)))
	//assert.Equal(t, []int{1}, fs1(strings.NewReader(`109,1,3,3,204,2,99`)))
	assert.Equal(t, []int{1}, fs1(strings.NewReader(`109,1,203,2,204,2,99`)))
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, []int{42}, fs1(f))
}

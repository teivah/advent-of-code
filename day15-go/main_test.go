package day15_go

import (
	"fmt"
	"os"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestFs1Test(t *testing.T) {
	f, err := os.Open("test.txt")
	require.NoError(t, err)
	v, err := fn1(f, 10)
	require.NoError(t, err)
	assert.Equal(t, 26, v)
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	v, err := fn1(f, 2000000)
	require.NoError(t, err)
	assert.Equal(t, 95437, v)
}

func TestFs2Test(t *testing.T) {
	f, err := os.Open("test.txt")
	require.NoError(t, err)
	v, err := fn2(f, 20)
	require.NoError(t, err)
	assert.Equal(t, 56000011, v)
}

func TestFs2Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	v, err := fn2(f, 4000000)
	require.NoError(t, err)
	assert.Equal(t, 11645454855041, v)
}

func TestRow(t *testing.T) {
	r := row{}
	r.addInterval(5, 10, beaconItem)
	r.addInterval(9, 11, beaconItem)
	r.addInterval(12, 14, beaconItem)
	fmt.Printf("%v\n", r)
}

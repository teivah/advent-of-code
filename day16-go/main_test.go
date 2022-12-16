package day15_go

import (
	"fmt"
	"os"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestToValve(t *testing.T) {
	valve, err := toValve("Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE")
	require.NoError(t, err)
	fmt.Printf("%v\n", valve)
}

func TestFs1Test(t *testing.T) {
	f, err := os.Open("test.txt")
	require.NoError(t, err)
	v, err := fn1(f)
	require.NoError(t, err)
	assert.Equal(t, 1651, v)
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	v, err := fn1(f)
	require.NoError(t, err)
	assert.Equal(t, 1651, v)
}

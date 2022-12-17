package day15_go

import (
	"fmt"
	"io"
	"os"
	"strings"
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
	test(t, fileReader(t, "test.txt"), 2, 0)
	test(t, fileReader(t, "test.txt"), 3, 20)
	test(t, fileReader(t, "test.txt"), 4, 40)
	test(t, fileReader(t, "test.txt"), 5, 63)
	test(t, fileReader(t, "test.txt"), 10, 246)
	test(t, fileReader(t, "test.txt"), 30, 1651)
}

func fileReader(t *testing.T, s string) io.Reader {
	f, err := os.Open(s)
	require.NoError(t, err)
	return f
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	v, err := fn1(f, 30)
	require.NoError(t, err)
	assert.Equal(t, 1651, v)
}

func TestFs1Tests(t *testing.T) {
	test(t, strings.NewReader(`Valve AA has flow rate=10; tunnels lead to valve BB
Valve BB has flow rate=50; tunnels lead to valves AA`), 2, 10)
	test(t, strings.NewReader(`Valve AA has flow rate=10; tunnels lead to valve BB
Valve BB has flow rate=50; tunnels lead to valves AA`), 3, 50)
	test(t, strings.NewReader(`Valve AA has flow rate=10; tunnels lead to valve BB
Valve BB has flow rate=50; tunnels lead to valves AA`), 5, 160)
}

func test(t *testing.T, r io.Reader, depth int, expected int) {
	v, err := fn1(r, depth)
	require.NoError(t, err)
	assert.Equal(t, expected, v)
}

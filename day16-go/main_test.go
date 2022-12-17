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

func TestFs1UnitTest(t *testing.T) {
	test1(t, fileReader(t, "test.txt"), 2, 0)
	test1(t, fileReader(t, "test.txt"), 3, 20)
	test1(t, fileReader(t, "test.txt"), 4, 40)
	test1(t, fileReader(t, "test.txt"), 5, 63)
	test1(t, fileReader(t, "test.txt"), 10, 246)
}

func TestFs1Unit(t *testing.T) {
	test1(t, fileReader(t, "test.txt"), 30, 1651)
}

func fileReader(t *testing.T, s string) io.Reader {
	f, err := os.Open(s)
	require.NoError(t, err)
	return f
}

func TestFs1Input(t *testing.T) {
	test1(t, fileReader(t, "input.txt"), 30, 1741)
}

func TestFs1Tests(t *testing.T) {
	//test1(t, strings.NewReader(`Valve AA has flow rate=10; tunnels lead to valve BB
	//Valve BB has flow rate=50; tunnels lead to valves AA`), 2, 10)
	//test1(t, strings.NewReader(`Valve AA has flow rate=10; tunnels lead to valve BB
	//Valve BB has flow rate=50; tunnels lead to valves AA`), 3, 50)
	//	test1(t, strings.NewReader(`Valve AA has flow rate=10; tunnels lead to valve BB
	//Valve BB has flow rate=50; tunnels lead to valves AA`), 5, 160)

	test1(t, strings.NewReader(`Valve AA has flow rate=40; tunnels lead to valve BB
Valve BB has flow rate=10; tunnels lead to valves AA, CC
Valve CC has flow rate=50; tunnels lead to valves BB`), 7, 400)

	test1(t, strings.NewReader(`Valve AA has flow rate=10; tunnels lead to valves BB, DD
Valve BB has flow rate=10; tunnels lead to valves AA, CC
Valve CC has flow rate=50; tunnels lead to valves BB
Valve DD has flow rate=50; tunnels lead to valves AA`), 4, 100)
}

func test1(t *testing.T, r io.Reader, depth int, expected int) {
	v, err := fn1(r, depth)
	require.NoError(t, err)
	assert.Equal(t, expected, v)
}

func TestFs2Unit(t *testing.T) {
	test2(t, fileReader(t, "test.txt"), 26, 1707)
	//test2(t, fileReader(t, "test.txt"), 12, 40)
}

func TestConsistency(t *testing.T) {
	for i := 0; i < 100; i++ {
		test2(t, fileReader(t, "test.txt"), 10, 414)
	}
}

func TestFs2Tests(t *testing.T) {
	//	test2(t, strings.NewReader(`Valve AA has flow rate=10; tunnels lead to valve BB
	//Valve BB has flow rate=50; tunnels lead to valves AA`), 2, 10)
	//	test2(t, strings.NewReader(`Valve AA has flow rate=10; tunnels lead to valve BB
	//Valve BB has flow rate=50; tunnels lead to valves AA`), 3, 70)
	//	test2(t, strings.NewReader(`Valve AA has flow rate=10; tunnels lead to valves BB, DD
	//Valve BB has flow rate=10; tunnels lead to valves AA, CC
	//Valve CC has flow rate=50; tunnels lead to valves BB
	//Valve DD has flow rate=50; tunnels lead to valves AA`), 3, 70)

	test2(t, strings.NewReader(`Valve AA has flow rate=10; tunnels lead to valves BB, DD
Valve BB has flow rate=10; tunnels lead to valves AA, CC
Valve CC has flow rate=50; tunnels lead to valves BB
Valve DD has flow rate=50; tunnels lead to valves AA`), 4, 150)
}

func test2(t *testing.T, r io.Reader, depth int, expected int) {
	v, err := fn2(r, depth)
	require.NoError(t, err)
	require.Equal(t, expected, v)
}

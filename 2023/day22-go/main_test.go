package main

import (
	"fmt"
	"os"
	"runtime"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestFs1Test(t *testing.T) {
	f, err := os.Open("test.txt")
	require.NoError(t, err)
	assert.Equal(t, 5, fs1(f))
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 499, fs1(f))
}

func TestFs2Test(t *testing.T) {
	f, err := os.Open("test.txt")
	require.NoError(t, err)
	assert.Equal(t, 7, fs2(f))
}

func TestFs2Input(t *testing.T) {
	fmt.Println("Memory usage before allocating nil slices:")
	PrintMemUsage()

	AllocNilSlices(1000000)

	fmt.Println("Memory usage after allocating nil slices:")
	PrintMemUsage()

	AllocEmptySlices(1000000)

	fmt.Println("Memory usage after allocating empty slices:")
	PrintMemUsage()
}

func AllocNilSlices(n int) {
	for i := 0; i < n; i++ {
		var s []int
		_ = s
	}
}

func AllocEmptySlices(n int) {
	for i := 0; i < n; i++ {
		s := make([]int, 0)
		_ = s
	}
}

func PrintMemUsage() {
	var m runtime.MemStats
	runtime.ReadMemStats(&m)
	fmt.Printf("Alloc = %v MiB", m.Alloc)
	fmt.Println()
}

package main

import (
	"fmt"
	"os"
	"sort"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestFs1Test(t *testing.T) {
	f, err := os.Open("test.txt")
	require.NoError(t, err)
	assert.Equal(t, 2024, fs1(f))
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 50411513338638, fs1(f))
}

func TestFs2Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 42, fs2(f))
}

func TestName(t *testing.T) {
	for i := 0; i < 100; i++ {
		for j := 0; j < 100; j++ {
			if add(i, j) != i+j {
				panic(fmt.Sprintf("%d %d", i, j))
			}
		}
	}
	//add(1, 0)
}

func TestFound(t *testing.T) {
	s := []string{"vwr", "z06", "tqm", "z11", "kfs", "z16", "gfv", "hcm"}
	sort.Strings(s)
	fmt.Println(strings.Join(s, ","))
}

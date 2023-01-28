package main

import (
	"fmt"
	"os"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestFs1Test(t *testing.T) {
	assert.Equal(t, 43210, fs1(strings.NewReader(`3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0`)))
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 255840, fs1(f))
}

func TestFs2Test(t *testing.T) {
	assert.Equal(t, 139629729, fs2(strings.NewReader(`3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5`)))
	//assert.Equal(t, 18216, fs2(strings.NewReader(`3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10`)))
}

func Test_permutations(t *testing.T) {
	fmt.Println(permutations(0, []int{1, 2, 3}))
}

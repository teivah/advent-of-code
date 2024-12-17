package main

import (
	"fmt"
	"math"
	"os"
	"strconv"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestFs1Test(t *testing.T) {
	f, err := os.Open("test.txt")
	require.NoError(t, err)
	assert.Equal(t, "4,6,3,5,6,3,5,2,1,0", fs1(f))
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, "4,0,4,7,1,2,7,1,6", fs1(f))
}

func TestFs2Test(t *testing.T) {
	f, err := os.Open("test2.txt")
	require.NoError(t, err)
	assert.Equal(t, 117440, fs2(f))
}

func TestFs2Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 42, fs2(f))
}

func TestName(t *testing.T) {
	solve()
}

func TestName2(t *testing.T) {
	n := 9
	search := []int{3, 1, 4, 4, 5, 5, 5, 3, 0}
	for i := 0; i < digits(3*n); i++ {
		var res []int
		var c int
		a := i
		found := true
		for j := 0; j < n; j++ {
			c, a = calc(a)
			res = append(res, c)
			if search[j] != c {
				found = false
				break
			}
		}
		if found {
			fmt.Printf("%d %s\n", i, binary(i, n))
		}
	}
}

func TestName3(t *testing.T) {
	m := 2097151
	checks := []string{
		"101110000000001011010111101",
		"101110000001001011010111101",
		"101110000100001011010111101",
		"101110001001001011010111101",
		"101110001100001011010111101",
	}
	search := []int{2, 4, 1, 1, 7, 5, 0, 3, 1, 4, 4, 5, 5, 5, 3, 0}
	res := math.MaxInt
	for _, check := range checks {
		for i := 0; i <= m; i++ {
			d := convert(check, i)
			a := d
			var c int
			found := true
			var r []int
			for j := 0; j < 16; j++ {
				c, a = calc(a)
				r = append(r, c)
				if search[j] != c {
					found = false
					break
				}
			}
			if found && a == 0 {
				res = min(res, d)
				fmt.Println(d, r)
			}
		}
	}
	fmt.Println(res)
}

func TestNamex(t *testing.T) {
	fmt.Println(isGood([]int{2, 4, 1, 1, 7, 5, 0, 3, 1, 4, 4, 5, 5, 5, 3, 0}, 130674566075152))
}

func isGood(search []int, a int) bool {
	var c int
	found := true
	for j := 0; j < len(search); j++ {
		c, a = calc(a)
		if search[j] != c {
			found = false
			break
		}
	}
	if found && a == 0 {
		return true
	}
	return false
}

func convert(prefix string, i int) int {
	s := fmt.Sprintf("%s%021b", prefix, i)
	d, err := strconv.ParseInt(s, 2, 0)
	if err != nil {
		panic(err)
	}
	return int(d)
}

func binary(i, n int) string {
	s := fmt.Sprintf("%048b", i)
	return s[len(s)-(n*3):]
}

func digits(x int) int {
	s := strings.Repeat("1", x)
	result, _ := strconv.ParseInt(s, 2, 64)
	return int(result)
}

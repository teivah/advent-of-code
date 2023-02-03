package main

import (
	"os"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestFs1Test1(t *testing.T) {
	assert.Equal(t, 35, fs1(strings.NewReader(`16
10
15
5
1
11
7
19
6
12
4`)))
}

func TestFs1Test2(t *testing.T) {
	assert.Equal(t, 220, fs1(strings.NewReader(`28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3`)))
}

func TestFs2Test0(t *testing.T) {
	assert.Equal(t, 4, fs2(strings.NewReader(`1
2
3`)))
}

func TestFs2Test1(t *testing.T) {
	assert.Equal(t, 8, fs2(strings.NewReader(`16
10
15
5
1
11
7
19
6
12
4`)))
}

func TestFs2Test2(t *testing.T) {
	assert.Equal(t, 19208, fs2(strings.NewReader(`28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3`)))
}

func TestFs2Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 10578455953408, fs2(f))
}

package main

import (
	"os"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestFs1Test(t *testing.T) {
	assert.Equal(t, "baedc", fs1("abcde", strings.NewReader("s1,x3/4,pe/b")))
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, "glnacbhedpfjkiom", fs1("abcdefghijklmnop", f))
}

func TestFs2Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, "fmpanloehgkdcbji", fs2(1_000_000_000, "abcdefghijklmnop", f))
}

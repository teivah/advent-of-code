package main

import (
	"os"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestFs1Test(t *testing.T) {
	assert.Equal(t, 3, fs1(strings.NewReader(`^WNE$`)))
	assert.Equal(t, 10, fs1(strings.NewReader(`^ENWWW(NEEE|SSE(EE|N))$`)))
	assert.Equal(t, 18, fs1(strings.NewReader(`^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$`)))
	assert.Equal(t, 23, fs1(strings.NewReader(`^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$`)))
	assert.Equal(t, 31, fs1(strings.NewReader(`^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$`)))
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 3675, fs1(f))
}

func TestFs2Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 42, fs2(f))
}

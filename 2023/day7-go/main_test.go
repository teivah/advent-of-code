package main

import (
	"fmt"
	"os"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestFs1Test(t *testing.T) {
	f, err := os.Open("test.txt")
	require.NoError(t, err)
	assert.Equal(t, 6440, fs1(f))
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 248836197, fs1(f))
}

func TestFs2Test(t *testing.T) {
	f, err := os.Open("test.txt")
	require.NoError(t, err)
	assert.Equal(t, 5905, fs2(f))
}

func TestFs2Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 251195607, fs2(f))
}

func Test_toHandType2(t *testing.T) {
	t.Parallel()

	cases := []struct {
		hand     string
		wantResp HandType
	}{
		{hand: "32T3K", wantResp: onePair},
		{hand: "JQQQA", wantResp: fourOfAKind},
		{hand: "JJQQQ", wantResp: fiveOfAKind},
		{hand: "JQQQQ", wantResp: fiveOfAKind},
		{hand: "JJJQQ", wantResp: fiveOfAKind},
		{hand: "JJJJQ", wantResp: fiveOfAKind},
		{hand: "JJJJJ", wantResp: fiveOfAKind},
		{hand: "JKKQQ", wantResp: fullHouse},
		{hand: "JJKQQ", wantResp: fourOfAKind},
		{hand: "234JJ", wantResp: threeOfAKind},
		{hand: "23JJJ", wantResp: fourOfAKind},
		{hand: "22JJJ", wantResp: fiveOfAKind},
	}

	for _, tc := range cases {
		tc := tc
		t.Run(fmt.Sprintf("%v", tc.hand), func(t *testing.T) {
			// WHEN
			assert.Equal(t, tc.wantResp, toHandType2(toPlayer(tc.hand+" 10").hand))
		})
	}
}

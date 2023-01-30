package main

import (
	"os"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestFs1Test(t *testing.T) {
	assert.Equal(t, "01029498", fs1(strings.NewReader(`12345678`), 4))
	assert.Equal(t, "24176176", fs1(strings.NewReader(`80871224585914546619083218645595`), 100))
	assert.Equal(t, "73745418", fs1(strings.NewReader(`19617804207202209144916044189917`), 100))
	assert.Equal(t, "52432133", fs1(strings.NewReader(`69317163492948606335995924319873`), 100))
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 42, fs1(f, 100))
}

func TestFs2Test(t *testing.T) {
	assert.Equal(t, "84462026", fs2(strings.NewReader(`03036732577212944063491565474664`), 100, 10000))
	//assert.Equal(t, "78725270", fs2(strings.NewReader(`02935109699940807407585447034323`), 100, 10000))
	//assert.Equal(t, "53553731", fs2(strings.NewReader(`03081770884921959731165446850517`), 100, 10000))
}

func TestFs2Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	//assert.Equal(t, 42, fs2(f, 100, 10000))
	assert.Equal(t, 42, fs2(f, 1, 100))
	//assert.Equal(t, 42, fs2(f, 100, 85))
	//assert.Equal(t, 42, fs2(f, 1, 1000))
}

/*
2, 150:
139 78491838
140 00590604
141 32699470
142 64798246
143 96897012
144 28996888
145 50095654
146 82194420
147 14293296
148 46392062
149 78491838

2, 151:
139 46392062
140 78491838
141 00590604
142 32699470
143 64798246
144 96897012
145 28996888
146 50095654
147 82194420
148 14293296
149 46392062

2, 152:
139 14293296

3, 150:
139 69178796
140 55501155
141 96489069
142 82812428
143 23790332
144 19123791
145 50001605
146 46434064
147 87312978
148 73745337
149 14623241
*/

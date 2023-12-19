package main

import (
	"os"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestFs1Test(t *testing.T) {
	f, err := os.Open("test.txt")
	require.NoError(t, err)
	assert.Equal(t, 19114, fs1(f))
}

func TestFs1Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 287054, fs1(f))
}

func TestFs2(t *testing.T) {
	t.Parallel()

	cases := []struct {
		input string
		resp  int
	}{
		{
			input: `in{x<2:x}
x{m<2:m}
m{a<2:a}
a{s<2:A}
`,
			resp: 1,
		},
		{
			input: `in{x<2:x}
x{m<2:m}
m{a<2:a}
a{s<3:A}
`,
			resp: 2,
		},
		{
			input: `in{x<2:x}
x{m<3:m}
m{a<2:a}
a{s<3:A}
`,
			resp: 4,
		},
		{
			input: `in{x>3999:x}
x{m>3999:m}
m{a>3999:a}
a{s>3999:A}
`,
			resp: 1,
		},
		{
			input: `in{x>3999:x}
x{m>3999:m}
m{a>3999:a}
a{s>3998:A}
`,
			resp: 2,
		},
		{
			input: `in{x>3990:x}
x{m>3990:m}
m{a>3990:a}
a{s>3990:A}
`,
			resp: 10000,
		},
		{
			input: `in{x>3999:x}
x{m>3999:m}
m{a>3999:a}
a{x<3999:A}
`,
			resp: 0,
		},
		{
			input: `in{x>3998:x}
x{m>3999:m}
m{a>3999:tmp}
tmp{s>3999:a}
a{x<4000:A}
`,
			resp: 1,
		},
		{
			input: `in{x<11:x,x>3990:x}
x{m>3999:m}
m{a>3999:a}
a{s>3999:A}
`,
			resp: 20,
		},
		{
			input: `in{x>3999:a}
a{m>3999:b}
b{a>3999:c}
c{s>3999:d}
d{e}
e{A}
`,
			resp: 1,
		},
		{
			input: `in{x>3999:a}
a{m>3999:b}
b{a>3999:c}
c{s>3999:d}
d{e}
e{R}
`,
			resp: 0,
		},
		{
			input: `in{x<2:a1}
a1{m>3999:b1}
b1{a>3999:c1}
c1{s>3999:A}`,
			resp: 1,
		},
		{
			input: `in{x<2:a1,x>3999:a1}
a1{m>3999:b1}
b1{a>3999:c1}
c1{s>3999:A}`,
			resp: 2,
		},
		{
			input: `in{x<2:a1,x>3999:a2}
a1{m>3999:b1}
b1{a>3999:c1}
c1{s>3999:A}
a2{m<2:b2}
b2{a<2:c2}
c2{s<2:A}`,
			resp: 2,
		},
		{
			input: `in{x<3:a1,x>3998:a2}
a1{m>3998:b1}
b1{a>3999:c1}
c1{s>3999:A}
a2{m<3:b2}
b2{a<2:c2}
c2{s<2:A}`,
			resp: 8,
		},
		{
			input: `in{x>0:a}
a{m>3999:b}
b{a>3999:c}
c{s>3999:A}`,
			resp: 4000,
		},
		{
			input: `in{x>0:a}
a{m>3998:b}
b{a>3999:c}
c{s>3999:A}`,
			resp: 8000,
		},
		{
			input: `in{x>0:a}
a{m>0:b}
b{a>3999:c}
c{s>3999:A}`,
			resp: 16000000,
		},
		{
			input: `in{x>0:a}
a{x<4000:R,m>3999:b}
b{a>3999:c}
c{s>3999:A}`,
			resp: 1,
		},
		{
			input: `in{m<4000:rej,x>3999:a}
rej{R}
a{m>0:b}
b{a>3999:c}
c{s>3999:A}`,
			resp: 1,
		},
		{
			input: `in{x>3999:a}
a{a<3999:R,m>3999:b}
b{a>3998:c}
c{s>3999:A}`,
			resp: 2,
		},
	}

	for _, tc := range cases {
		tc := tc
		t.Run(tc.input, func(t *testing.T) {
			assert.Equal(t, tc.resp, fs2(strings.NewReader(tc.input)))
		})
	}
}

func TestFs2Test(t *testing.T) {
	f, err := os.Open("test.txt")
	require.NoError(t, err)
	assert.Equal(t, 167409079868000, fs2(f))
}

func TestFs2Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	assert.Equal(t, 42, fs2(f))
}

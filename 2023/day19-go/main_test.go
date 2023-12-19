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
		{
			input: `in{x>3999:a}
a{a<3999:rej,m>3999:b}
rej{R}
b{a>3998:c}
c{s>3999:A}`,
			resp: 2,
		},
		{
			input: `in{x>3999:a}
a{a>3999:R,m>3999:b}
b{a>3998:c}
c{s>3999:A}`,
			resp: 1,
		},
		{
			input: `in{x<4000:rej,m<4000:rej,a}
rej{R}
a{x>2000:c,m>2000:c}
c{a>3999:d}
d{s>3999:A}`,
			resp: 1,
		},
		{
			input: `in{x<100:a,x<201:a,a}
a{m>3999:b}
b{a>3999:c}
c{s>3999:A}`,
			resp: 4000,
		},
		{
			input: `in{x<101:a,x<201:a,x>3999:a}
a{m>3999:b}
b{a>3999:c}
c{s>3999:A}`,
			resp: 201,
		},
		{
			input: `in{x<101:a,x<102:b}
a{m>3999:b}
b{a>3999:c}
c{s>3999:A}`,
			resp: 4100,
		},
		{
			input: `in{x<4000:rej,a}
rej{R}
a{m>3999:b}
b{a>3999:c}
c{s>3999:A}`,
			resp: 1,
		},
		{
			input: `in{x<101:a1,a2}
a1{m>3999:b1}
b1{a>3999:c1}
c1{s>3999:A}
a2{m>3999:b2}
b2{a>3999:c2}
c2{s>3999:c3}
c3{x>100:A}`,
			resp: 4000,
		},
		{
			input: `in{x<4000:rej,a}
rej{R}
a{m>3999:b}
b{a>3999:c}
c{s>3999:d}
d{x>2000:A}`,
			resp: 1,
		},
		{
			input: `in{x<2:a,x<3:a,x<4:a,x>3999:a,rej}
rej{R}
a{m>3999:b}
b{a>3999:c}
c{s>3999:A}`,
			resp: 4,
		},
		{
			input: `in{x<2:a,x<3:a,x<4:a,x>3999:a,rej}
rej{R}
a{x<2:aa}
aa{m>3999:b}
b{a>3999:c}
c{s>3999:A}`,
			resp: 1,
		},
		{
			input: `in{x>2000:a}
a{m>3999:b}
b{a>3999:c}
c{s>3999:d}
d{x<2501:A}`,
			resp: 500,
		},
		{
			input: `in{x>2000:a,x<3001:a}
a{m>3999:b}
b{a>3999:c}
c{s>3999:d}
d{x<2501:A}`,
			resp: 2500,
		},
		{
			input: `in{x>3999:a}
a{m>3999:b}
b{a>3999:c,x>3999:c}
c{s>3999:app}
app{A}`,
			resp: 4000,
		},
		{
			input: `in{x>3999:a}
a{m>3999:b}
b{a>3999:c}
c{s>3999:app,A}
app{x<1000:A,R}`,
			resp: 3999,
		},
		{
			input: `in{x>3999:A}`,
			resp:  4000 * 4000 * 4000,
		},
		{
			input: `in{x>3999:a}
a{m>3999:A}`,
			resp: 4000 * 4000,
		},
		{
			input: `in{x>3999:a}
a{m>3999:b}
b{a>3999:A}
c{A}`,
			resp: 4000,
		},
		{
			input: `in{x>3999:a}
a{m>3999:b}
b{a>3999:c}
c{s>3999:A}`,
			resp: 1,
		},
		{
			input: `in{x>1:R,m>2:R,a>1:R,A}`,
			resp:  8000,
		},
		{
			input: `in{x<2:a1,a2}
a1{m<2:b1}
b1{a<2:c1}
c1{s<2:A}
a2{m<2:b2}
b2{a<2:c2}
c2{s<2:A}`,
			resp: 4000,
		},
		{
			input: `in{x<11:a,a}
a{x>3999:b}
b{m<2:c}
c{a<2:d}
d{s<2:A}`,
			resp: 1,
		},
		{
			input: `in{x<2001:A,R}`,
			resp:  128000000000000,
		},
		{
			input: `in{x<4000:R,m<4000:R,a<4000:R,s<4000:R,A}`,
			resp:  1,
		},
		{
			input: `in{x>1:R,m>1:R,a>1:R,s>1:R,A}`,
			resp:  1,
		},
		{
			input: `in{x>3:R,x<2:R,A}`,
			resp:  128000000000,
		},
		{
			input: `in{x<4000:R,m<4000:R,a<4000:R,s>2000:a}
a{x<3000:R,s>3000:A}`,
			resp: 1000,
		},
		{
			input: `in{x<11:a,m<2:a2}
a{m<2:b}
b{a<2:c}
c{s<2:A}
a2{x<12:b2}
b2{a>3999:c2}
c2{s<2:A}`,
			resp: 11,
		},
		{
			input: `in{x<4000:R,m<4000:R,a<4000:R,a}
a{s<2001:R,A}`,
			resp: 2000,
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
	assert.Equal(t, 131619440296497, fs2(f))
}

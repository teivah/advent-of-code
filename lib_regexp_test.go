package aoc_test

import (
	"regexp"
	"testing"

	"github.com/stretchr/testify/assert"
	aoc "github.com/teivah/advent-of-code"
)

func TestRegexpFindAll(t *testing.T) {
	re := regexp.MustCompile(`\d*`)
	s := "123,456"
	assert.Equal(t, []string{"123", "456"}, aoc.RegexpFindAll(s, re))
}

func TestRegexpFindIndices(t *testing.T) {
	re := regexp.MustCompile(`\d*`)
	s := "123,456"
	assert.Equal(t, []aoc.CapturingGroup{
		{0, 3},
		{4, 7},
	}, aoc.RegexpFindIndices(s, re))
}

func TestRegexpFindSubmatches(t *testing.T) {
	re := regexp.MustCompile(`mul\((\d{1,3}),(\d{1,3})\)`)
	s := "mul(1,2),mul(42,0)"
	assert.Equal(t, []aoc.Submatch{
		{
			Start: 0,
			End:   8,
			CapturingGroups: []aoc.CapturingGroup{
				{Start: 4, End: 5},
				{Start: 6, End: 7},
			},
		},
		{
			Start: 9,
			End:   18,
			CapturingGroups: []aoc.CapturingGroup{
				{Start: 13, End: 15},
				{Start: 16, End: 17},
			},
		},
	}, aoc.RegexpFindSubmatches(s, re))
}

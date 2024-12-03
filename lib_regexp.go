package aoc

import (
	"regexp"
)

// RegexpFindAll finds all occurrences of a regexp.
func RegexpFindAll(s string, re *regexp.Regexp) []string {
	return re.FindAllString(s, -1)
}

// RegexpFindIndices finds all the indices of a regexp.
func RegexpFindIndices(s string, re *regexp.Regexp) []CapturingGroup {
	var res []CapturingGroup
	for _, v := range re.FindAllStringIndex(s, -1) {
		res = append(res, CapturingGroup{v[0], v[1]})
	}
	return res
}

type Submatch struct {
	Start int
	End   int
	// CapturingGroups is a list of optional capturing groups.
	// For example, `mul\((\d{1,3}),(\d{1,3})\)` contains 2 capturing groups.
	CapturingGroups []CapturingGroup
}

type CapturingGroup struct {
	Start int
	End   int
}

// RegexpFindSubmatches find all submatches and related capturing groups.
func RegexpFindSubmatches(s string, re *regexp.Regexp) []Submatch {
	var res []Submatch
	for _, match := range re.FindAllStringSubmatchIndex(s, -1) {
		sub := Submatch{Start: match[0], End: match[1]}
		match = match[2:]
		for len(match) > 0 {
			sub.CapturingGroups = append(sub.CapturingGroups, CapturingGroup{
				Start: match[0],
				End:   match[1],
			})
			match = match[2:]
		}
		res = append(res, sub)
	}
	return res
}

package main

import (
	"io"
	"sort"

	"github.com/teivah/go-aoc"
)

func fs1(input io.Reader) int {
	groups := aoc.StringGroups(aoc.ReaderToStrings(input))
	var intervals [][2]int
	for _, line := range groups[0] {
		del := aoc.NewDelimiter(line, "-")
		intervals = append(intervals, [2]int{del.GetInt(0), del.GetInt(1)})
	}
	var ingredients []int
	for _, line := range groups[1] {
		ingredients = append(ingredients, aoc.StringToInt(line))
	}

	sort.Slice(intervals, func(i, j int) bool {
		a := intervals[i]
		b := intervals[j]
		if a[0] < b[0] {
			return true
		}
		if b[0] < a[0] {
			return false
		}
		return a[1] <= b[1]
	})

	var res [][2]int
	cur := intervals[0]
	for i := 1; i < len(intervals); i++ {
		next := intervals[i]
		if cur == next {
			continue
		}
		if isOverlap(cur, next) {
			cur = [2]int{cur[0], max(cur[1], next[1])}
		} else {
			res = append(res, cur)
			cur = next
		}
	}
	res = append(res, cur)

	count := 0
	for _, ing := range ingredients {
		for _, interval := range intervals {
			if ing >= interval[0] && ing <= interval[1] {
				count++
				break
			}
		}
	}
	return count
}

func isOverlap(a, b [2]int) bool {
	return b[0] >= a[0] && b[0] <= a[1]
}

func fs2(input io.Reader) int {
	groups := aoc.StringGroups(aoc.ReaderToStrings(input))
	var intervals [][2]int
	for _, line := range groups[0] {
		del := aoc.NewDelimiter(line, "-")
		intervals = append(intervals, [2]int{del.GetInt(0), del.GetInt(1)})
	}

	sort.Slice(intervals, func(i, j int) bool {
		a := intervals[i]
		b := intervals[j]
		if a[0] < b[0] {
			return true
		}
		if b[0] < a[0] {
			return false
		}
		return a[1] <= b[1]
	})

	var res [][2]int
	cur := intervals[0]
	for i := 1; i < len(intervals); i++ {
		next := intervals[i]
		if cur == next {
			continue
		}
		if isOverlap(cur, next) {
			cur = [2]int{cur[0], max(cur[1], next[1])}
		} else {
			res = append(res, cur)
			cur = next
		}
	}
	res = append(res, cur)

	count := 0
	for _, interval := range res {
		count += interval[1] - interval[0] + 1
	}
	return count
}

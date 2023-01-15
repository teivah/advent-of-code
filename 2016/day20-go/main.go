package main

import (
	"bufio"
	"io"
	"sort"
	"strconv"
	"strings"
)

func fs1(input io.Reader) (int, error) {
	scanner := bufio.NewScanner(input)
	var ranges []Range
	for scanner.Scan() {
		s := scanner.Text()
		idx := strings.Index(s, "-")
		min, err := strconv.Atoi(s[:idx])
		if err != nil {
			return 0, err
		}
		max, err := strconv.Atoi(s[idx+1:])
		if err != nil {
			return 0, err
		}
		ranges = append(ranges, Range{min, max})
	}

	sort.Slice(ranges, func(i, j int) bool {
		a := ranges[i]
		b := ranges[j]
		if a.min < b.min {
			return true
		}
		if b.max < a.max {
			return false
		}
		return a.max <= b.max
	})

	var merged []Range
	last := ranges[0]
	for i := 1; i < len(ranges); i++ {
		rng := ranges[i]
		if rng.min <= last.max || rng.min == last.max+1 {
			last = Range{last.min, max(last.max, rng.max)}
		} else {
			merged = append(merged, last)
			last = rng
		}
	}
	merged = append(merged, last)

	return merged[0].max + 1, nil
}

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

type Range struct {
	min int
	max int
}

func fs2(input io.Reader, maxRange int) (int, error) {
	scanner := bufio.NewScanner(input)
	var ranges []Range
	for scanner.Scan() {
		s := scanner.Text()
		idx := strings.Index(s, "-")
		min, err := strconv.Atoi(s[:idx])
		if err != nil {
			return 0, err
		}
		max, err := strconv.Atoi(s[idx+1:])
		if err != nil {
			return 0, err
		}
		ranges = append(ranges, Range{min, max})
	}

	sort.Slice(ranges, func(i, j int) bool {
		a := ranges[i]
		b := ranges[j]
		if a.min < b.min {
			return true
		}
		if b.min < a.min {
			return false
		}
		return a.max <= b.max
	})

	var merged []Range
	last := ranges[0]
	for i := 1; i < len(ranges); i++ {
		rng := ranges[i]
		if rng.min <= last.max || rng.min == last.max+1 {
			last = Range{last.min, max(last.max, rng.max)}
		} else {
			merged = append(merged, last)
			last = rng
		}
	}
	merged = append(merged, last)

	sum := 0
	for _, rng := range merged {
		sum += rng.max - rng.min + 1
	}

	return maxRange - sum + 1, nil
}

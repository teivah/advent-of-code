package main

import (
	"bufio"
	"fmt"
	"io"
	"math"
	"sort"
	"strconv"
	"strings"
)

func fs1(input io.Reader) (int, error) {
	scanner := bufio.NewScanner(input)
	var packages []int
	sum := 0
	for scanner.Scan() {
		s := scanner.Text()
		i, err := strconv.Atoi(s)
		if err != nil {
			return 0, err
		}
		packages = append(packages, i)
		sum += i
	}

	f := best(0, packages, 0, 0, 0, 0, 1, sum/3, nil, nil, nil)

	return f.qe, nil
}

type Found struct {
	nbPackages int
	qe         int
}

var cache map[string]struct{}

func formatKey(a ...any) string {
	sb := strings.Builder{}
	for _, s := range a {
		sb.WriteString(fmt.Sprintf("%v,", s))
	}
	return sb.String()
}

func addCache(a ...any) {
	key := formatKey(a...)
	if cache == nil {
		cache = make(map[string]struct{})
	}
	cache[key] = struct{}{}
}

func containsCache(a ...any) bool {
	key := formatKey(a...)
	_, exists := cache[key]
	return exists
}

func best(idx int, packages []int, w1, w2, w3, nbPackages1, qe, target int, s1, s2, s3 []int) Found {
	if idx == len(packages) {
		if w1 != w2 || w1 != w3 {
			return Found{math.MaxInt, math.MaxInt}
		}
		fmt.Println(qe, s1, s2, s3)
		return Found{nbPackages1, qe}
	}

	if containsCache(s1) {
		return Found{math.MaxInt, math.MaxInt}
	}

	if w1 > target || w2 > target || w3 > target {
		return Found{math.MaxInt, math.MaxInt}
	}

	weight := packages[idx]

	var found1 Found
	if multiplyWillOverflow(qe, weight) {
		found1 = Found{math.MaxInt, math.MaxInt}
	} else {
		found1 = best(idx+1, packages, w1+weight, w2, w3, nbPackages1+1, qe*weight, target, append(s1, weight), s2, s3)
	}
	found2 := best(idx+1, packages, w1, w2+weight, w3, nbPackages1, qe, target, s1, append(s2, weight), s3)
	found3 := best(idx+1, packages, w1, w2, w3+weight, nbPackages1, qe, target, s1, s2, append(s3, weight))

	s := []Found{found1, found2, found3}
	sort.Slice(s, func(i, j int) bool {
		a := s[i]
		b := s[j]

		if a.nbPackages < b.nbPackages {
			return true
		}
		if b.nbPackages < a.nbPackages {
			return false
		}
		return a.qe < b.qe
	})

	addCache(s1)

	return s[0]
}

func multiplyWillOverflow(x, y int) bool {
	if x <= 1 || y <= 1 {
		return false
	}
	d := x * y
	return d/y != x
}

func fs2(input io.Reader) (int, error) {
	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
		line := scanner.Text()
		_ = line
	}

	return 0, nil
}

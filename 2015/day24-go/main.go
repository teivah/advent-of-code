package main

import (
	"bufio"
	"io"
	"math"
	"sort"
	"strconv"
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

	f := best(0, packages, 0, 0, 0, 0, 1, sum)

	return f.qe, nil
}

type Found struct {
	nbPackages int
	qe         int
}

// idx, weight1, weight2, weight3
var cache map[int]map[int]map[int]map[int]struct{}

func addCache(idx, w1, w2, w3 int) {
	if cache == nil {
		cache = make(map[int]map[int]map[int]map[int]struct{})
	}

	v, exists := cache[idx]
	if !exists {
		v = make(map[int]map[int]map[int]struct{})
		cache[idx] = v
	}

	v2, exists := v[w1]
	if !exists {
		v2 = make(map[int]map[int]struct{})
		v[w1] = v2
	}

	v3, exists := v2[w2]
	if !exists {
		v3 = make(map[int]struct{})
		v2[w2] = v3
	}

	v3[w3] = struct{}{}
}

func containsCache(idx, w1, w2, w3 int) bool {
	if v, exists := cache[idx]; exists {
		if v2, exists := v[w1]; exists {
			if v3, exists := v2[w2]; exists {
				_, exists := v3[w3]
				return exists
			}
		}
	}
	return false
}

func best(idx int, packages []int, w1, w2, w3, nbPackages1, qe, remaining int) Found {
	if idx == len(packages) {
		if w1 != w2 || w1 != w3 {
			return Found{math.MaxInt, math.MaxInt}
		}
		return Found{nbPackages1, qe}
	}

	if 2*w1 > w2+w3+remaining {
		return Found{math.MaxInt, math.MaxInt}
	}

	if containsCache(idx, w1, w2, w3) || containsCache(idx, w1, w3, w2) {
		return Found{math.MaxInt, math.MaxInt}
	}

	addCache(idx, w1, w2, w3)

	weight := packages[idx]
	remaining -= weight

	var found1 Found
	if multiplyWillOverflow(qe, weight) {
		found1 = Found{math.MaxInt, math.MaxInt}
	} else {
		found1 = best(idx+1, packages, w1+weight, w2, w3, nbPackages1+1, qe*weight, remaining)
	}
	found2 := best(idx+1, packages, w1, w2+weight, w3, nbPackages1, qe, remaining)
	found3 := best(idx+1, packages, w1, w2, w3+weight, nbPackages1, qe, remaining)

	s := []Found{found1, found2, found3}
	sort.Slice(s, func(i, j int) bool {
		a := s[i]
		b := s[j]

		if a.nbPackages < b.nbPackages {
			return true
		}
		if b.nbPackages < b.nbPackages {
			return false
		}
		return a.qe < b.qe
	})

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

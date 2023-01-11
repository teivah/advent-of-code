package main

import (
	"bufio"
	"fmt"
	"io"
	"math"
	"math/big"
	"sort"
	"strconv"
	"strings"
)

/*
257645256986 [2 13 73 103 107 109 113] [1 3 7 11 17 19 23 31 37 41 43 47 53 59 61 67] [71 79 83 89 97 101]
*/
func fs1(input io.Reader) (string, error) {
	scanner := bufio.NewScanner(input)
	var packages []int
	sum := 0
	for scanner.Scan() {
		s := scanner.Text()
		i, err := strconv.Atoi(s)
		if err != nil {
			return "", err
		}
		packages = append(packages, i)
		sum += i
	}

	f := best(0, packages, 0, 0, 0, 0, big.NewInt(1), sum/3)

	return f.qe.String(), nil
}

type Found struct {
	nbPackages int
	qe         *big.Int
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

// 257645256986
func best(idx int, packages []int, w1, w2, w3, nbPackages1 int, qe *big.Int, target int) Found {
	if idx == len(packages) {
		if w1 != w2 || w1 != w3 {
			return Found{math.MaxInt, new(big.Int)}
		}
		return Found{nbPackages1, qe}
	}

	if w1 > target || w2 > target || w3 > target {
		return Found{math.MaxInt, new(big.Int)}
	}

	weight := packages[idx]

	var found1 Found
	result := new(big.Int)
	result.Mul(qe, big.NewInt(int64(weight)))
	found1 = best(idx+1, packages, w1+weight, w2, w3, nbPackages1+1, result, target)
	found2 := best(idx+1, packages, w1, w2+weight, w3, nbPackages1, qe, target)
	found3 := best(idx+1, packages, w1, w2, w3+weight, nbPackages1, qe, target)

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
		return a.qe.Cmp(b.qe) == -1
	})

	return s[0]
}

func fs2(input io.Reader) (int, error) {
	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
		line := scanner.Text()
		_ = line
	}

	return 0, nil
}

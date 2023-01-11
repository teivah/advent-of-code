package main

import (
	"bufio"
	"io"
	"math"
	"math/big"
	"strconv"
)

func fs(input io.Reader, groups int) (string, error) {
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

	solutions := all(0, packages, 0, sum/groups, nil)
	min := math.MaxInt
	for _, solution := range solutions {
		min = getMin(min, len(solution))
	}

	var mingQE *big.Int
	for _, solution := range solutions {
		if len(solution) == min {
			v := qe(solution)
			if mingQE == nil {
				mingQE = v
			} else if v.Cmp(mingQE) == -1 {
				mingQE = v
			}
		}
	}

	return mingQE.String(), nil
}

func qe(solution []int) *big.Int {
	res := big.NewInt(1)
	res.SetString("1", 10)
	for _, i := range solution {
		n := new(big.Int)
		n.Mul(res, big.NewInt(int64(i)))
		res = n
	}
	return res
}

func getMin(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func all(idx int, packages []int, w, target int, current []int) [][]int {
	if w == target {
		return [][]int{current}
	}

	if idx == len(packages) || w > target {
		return nil
	}

	weight := packages[idx]
	var res [][]int
	// With
	res = append(res, all(idx+1, packages, w+weight, target, append(current, weight))...)
	// Without
	res = append(res, all(idx+1, packages, w, target, current)...)

	return res
}

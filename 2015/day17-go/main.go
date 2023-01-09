package main

import (
	"bufio"
	"io"
	"math"
	"strconv"
)

func fs1(input io.Reader, v int) (int, error) {
	scanner := bufio.NewScanner(input)
	var containers []int
	for scanner.Scan() {
		line := scanner.Text()
		i, err := strconv.Atoi(line)
		if err != nil {
			return 0, err
		}
		containers = append(containers, i)
	}

	allunique = make(map[int]struct{})
	allcache = make(map[int]struct{})

	return all(v, containers, 0), nil
}

var allunique map[int]struct{}
var allcache map[int]struct{}

func all(remaining int, containers []int, key int) int {
	if remaining == 0 {
		if _, exists := allunique[key]; exists {
			return 0
		}
		allunique[key] = struct{}{}
		return 1
	}

	if _, exists := allcache[key]; exists {
		return 0
	}

	sum := 0
	for i, container := range containers {
		if container == 0 {
			continue
		}
		if container <= remaining {
			old := containers[i]
			containers[i] = 0
			sum += all(remaining-container, containers, key+(1<<i))
			containers[i] = old
		}
	}

	allcache[key] = struct{}{}
	return sum
}

func fs2(input io.Reader, v int) (int, error) {
	scanner := bufio.NewScanner(input)
	var containers []int
	for scanner.Scan() {
		line := scanner.Text()
		i, err := strconv.Atoi(line)
		if err != nil {
			return 0, err
		}
		containers = append(containers, i)
	}

	mincache = make(map[int]int)
	ways = make(map[int]map[int]struct{})

	best := min(v, containers, 0, 0)
	return len(ways[best]), nil
}

var mincache map[int]int

// 3 -> listKeys
var ways map[int]map[int]struct{}

func min(remaining int, containers []int, key int, cur int) int {
	if remaining == 0 {
		v, exists := ways[cur]
		if !exists {
			v = make(map[int]struct{})
			ways[cur] = v
			v[key] = struct{}{}
		} else {
			if _, exists := v[key]; !exists {
				v[key] = struct{}{}
			}
		}

		return cur
	}

	if v, exists := mincache[key]; exists {
		return v
	}

	best := math.MaxInt
	found := false
	for i, container := range containers {
		if container == 0 {
			continue
		}
		if container <= remaining {
			old := containers[i]
			containers[i] = 0
			v := min(remaining-container, containers, key+(1<<i), cur+1)
			if v != 0 {
				found = true
				best = getmin(best, v)
			}
			containers[i] = old
		}
	}

	if !found {
		best = math.MaxInt
	}
	mincache[key] = best
	return best
}

func getmin(a, b int) int {
	if a < b {
		return a
	}
	return b
}

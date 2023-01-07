package main

import (
	"bufio"
	"io"
	"math"
	"strconv"
	"strings"
)

func fs1(input io.Reader) (int, error) {
	scanner := bufio.NewScanner(input)

	r := Routes{
		distances: make(map[string]map[string]int),
	}

	for scanner.Scan() {
		s := scanner.Text()
		idx := indexAll(s, " ")
		from := s[:idx[0]]
		to := s[idx[1]+1 : idx[2]]
		distance, err := strconv.Atoi(s[idx[3]+1:])
		if err != nil {
			return 0, err
		}
		r.addRoute(from, to, distance)
	}

	visited := make(map[string]bool)
	for city := range r.distances {
		visited[city] = false
	}

	return r.shortestDistance(visited, "", 0), nil
}

type Routes struct {
	distances map[string]map[string]int
}

func (r *Routes) shortestDistance(visited map[string]bool, currentCity string, sum int) int {
	allVisited := true
	for _, isVisited := range visited {
		if !isVisited {
			allVisited = false
			break
		}
	}
	if allVisited {
		return sum
	}

	best := math.MaxInt
	for city, isVisited := range visited {
		if isVisited {
			continue
		}

		visited[city] = true
		v := 0
		if currentCity == "" {
			v = r.shortestDistance(visited, city, 0)
		} else {
			v = r.shortestDistance(visited, city, sum+r.getDistance(currentCity, city))
		}
		best = min(best, v)
		visited[city] = false
	}

	return best
}

func (r *Routes) longestDistance(visited map[string]bool, currentCity string, sum int) int {
	allVisited := true
	for _, isVisited := range visited {
		if !isVisited {
			allVisited = false
			break
		}
	}
	if allVisited {
		return sum
	}

	best := -1
	for city, isVisited := range visited {
		if isVisited {
			continue
		}

		visited[city] = true
		v := 0
		if currentCity == "" {
			v = r.longestDistance(visited, city, 0)
		} else {
			v = r.longestDistance(visited, city, sum+r.getDistance(currentCity, city))
		}
		best = max(best, v)
		visited[city] = false
	}

	return best
}

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

func (r *Routes) getDistance(from string, to string) int {
	return r.distances[from][to]
}

func (r *Routes) addRoute(from string, to string, distance int) {
	v, exists := r.distances[from]
	if !exists {
		v = make(map[string]int)
		r.distances[from] = v
	}
	v[to] = distance

	v, exists = r.distances[to]
	if !exists {
		v = make(map[string]int)
		r.distances[to] = v
	}
	v[from] = distance
}

func fs2(input io.Reader) (int, error) {
	scanner := bufio.NewScanner(input)

	r := Routes{
		distances: make(map[string]map[string]int),
	}

	for scanner.Scan() {
		s := scanner.Text()
		idx := indexAll(s, " ")
		from := s[:idx[0]]
		to := s[idx[1]+1 : idx[2]]
		distance, err := strconv.Atoi(s[idx[3]+1:])
		if err != nil {
			return 0, err
		}
		r.addRoute(from, to, distance)
	}

	visited := make(map[string]bool)
	for city := range r.distances {
		visited[city] = false
	}

	return r.longestDistance(visited, "", 0), nil
}

func indexAll(s string, search string) []int {
	i := 0
	var res []int
	for i < len(s) {
		index := strings.Index(s[i:], search)
		if index == -1 {
			return res
		}
		res = append(res, index+i)
		i += index + len(search)
	}
	return res
}

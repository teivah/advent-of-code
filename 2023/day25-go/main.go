package main

import (
	"bufio"
	"fmt"
	"io"
	"math"

	aoc "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	graph := make(map[string]map[string]bool)
	set := make(map[string]struct{})
	for scanner.Scan() {
		line := scanner.Text()

		from, tos := parse(line)
		set[from] = struct{}{}
		for _, to := range tos {
			add(graph, from, to)
			add(graph, to, from)
			set[to] = struct{}{}
		}
	}

	distances := make(map[string]int)
	minDistance := math.MaxInt
	i := 0
	for node := range set {
		i++
		fmt.Println(i, len(set))
		visited := make(map[string]int)
		dfs(graph, node, visited, 0)
		best := 0
		for _, d := range visited {
			best = max(best, d)
		}
		distances[node] = best
		minDistance = min(minDistance, best)
	}

	// Not sure if my solution is generic enough but in a nutshell, my intuition
	// is that we can be found the nodes where we have to cut an edge by
	// calculating the max distance to another node in the graph. Indeed, if
	// if a node has a minimal max distance, then it means it's "in the middle" of
	// the graph.
	found := make(map[string]struct{})
	for node, distance := range distances {
		if distance == minDistance {
			found[node] = struct{}{}
		}
	}

	if len(found) != 6 {
		panic("no result found")
	}

	var length [][2]string
	deleted := make(map[string]bool)
	for len(found) != 0 {
		for from := range found {
			delete(found, from)
			for to := range found {
				if _, exists := graph[from][to]; exists {
					length = append(length, [2]string{from, to})
					graph[from][to] = false
					graph[to][from] = false
					deleted[from] = true
					deleted[to] = true
				}
			}
		}
	}

	v1 := 0
	for k := range set {
		if deleted[k] {
			continue
		}

		visited := make(map[string]int)
		_ = dfs(graph, k, visited, 0)
		v := len(visited)
		if v1 == 0 {
			v1 = v
		} else {
			if v != v1 {
				return v1 * v
			}
		}
	}

	return 42
}

func dfs(graph map[string]map[string]bool, node string, visited map[string]int, distance int) int {
	if v, contains := visited[node]; contains {
		if v <= distance {
			return 0
		}
	}
	visited[node] = distance

	tos := graph[node]
	if len(tos) == 0 {
		return distance
	}

	best := 0
	for to, exists := range tos {
		if !exists {
			continue
		}
		best = max(best, dfs(graph, to, visited, distance+1))
	}
	return best
}

func add(graph map[string]map[string]bool, from, to string) {
	if _, exists := graph[from]; !exists {
		graph[from] = make(map[string]bool)
	}
	graph[from][to] = true
}

func parse(s string) (string, []string) {
	del := aoc.NewDelimiter(s, ": ")
	from := del.GetString(0)
	return from, aoc.NewDelimiter(del.GetString(1), " ").GetStrings()
}

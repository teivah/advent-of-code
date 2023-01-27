package main

import (
	"bufio"
	"io"
	"strings"
)

func fs1(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	orbits := make(map[string]string)
	for scanner.Scan() {
		line := scanner.Text()
		a, b := toOrbit(line)
		orbits[b] = a
	}

	sum := 0
	for k := range orbits {
		sum += length(orbits, k)
	}

	return sum
}

func length(orbits map[string]string, planet string) int {
	v, exists := orbits[planet]
	if !exists {
		return 0
	}
	return 1 + length(orbits, v)
}

func toOrbit(s string) (string, string) {
	idx := strings.Index(s, ")")
	return s[:idx], s[idx+1:]
}

func fs2(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	orbitFrom := make(map[string]string)
	orbitTo := make(map[string][]string)
	for scanner.Scan() {
		line := scanner.Text()
		a, b := toOrbit(line)
		orbitFrom[b] = a
		orbitTo[a] = append(orbitTo[a], b)
	}

	return bfs(orbitFrom["YOU"], orbitFrom["SAN"], orbitFrom, orbitTo)
}

func bfs(from string, to string, orbitFrom map[string]string, orbitTo map[string][]string) int {
	type State struct {
		move     int
		position string
	}
	q := []State{{0, from}}
	visited := make(map[string]bool)

	for len(q) != 0 {
		s := q[0]
		q = q[1:]

		if s.position == to {
			return s.move
		}

		if visited[s.position] {
			continue
		}
		visited[s.position] = true

		if planet, exists := orbitFrom[s.position]; exists {
			q = append(q, State{s.move + 1, planet})
		}

		if planets, exists := orbitTo[s.position]; exists {
			for _, planet := range planets {
				q = append(q, State{s.move + 1, planet})
			}
		}
	}

	return -1
}

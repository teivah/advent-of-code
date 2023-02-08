package main

import (
	"io"
	"math"

	aoc "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	lines := aoc.ReaderToStrings(input)
	g := toGraph(lines)

	return g.dfs("start", map[string]bool{"start": true})
}

func (g Graph) dfs(cur string, visited map[string]bool) int {
	if cur == "end" {
		return 1
	}

	edges := g.grid[cur]
	sum := 0
	for _, edge := range edges {
		if visited[edge] {
			continue
		}

		if g.caves[edge].small {
			visited[edge] = true
		}
		sum += g.dfs(edge, visited)
		visited[edge] = false
	}
	return sum
}

func toGraph(lines []string) Graph {
	grid := make(map[string][]string)
	caves := make(map[string]Cave)
	for _, line := range lines {
		del := aoc.NewDelimiter(line, "-")
		from := del.GetString(0)
		to := del.GetString(1)

		caves[from] = Cave{
			name:  from,
			small: isSmallCave(from),
		}
		caves[to] = Cave{
			name:  to,
			small: isSmallCave(to),
		}

		grid[from] = append(grid[from], to)
		grid[to] = append(grid[to], from)
	}
	return Graph{
		grid:  grid,
		caves: caves,
	}
}

func isSmallCave(s string) bool {
	r := s[0]
	return r >= 'a' && r <= 'z'
}

type Graph struct {
	grid  map[string][]string
	caves map[string]Cave
}

type Cave struct {
	name  string
	small bool
}

func fs2(input io.Reader) int {
	lines := aoc.ReaderToStrings(input)
	g := toGraph(lines)

	res := make(map[string]struct{})
	for _, c := range g.caves {
		if !c.small {
			continue
		}
		remaining := make(map[string]int)
		for _, cave := range g.caves {
			if cave.small {
				if c == cave {
					remaining[cave.name] = 2
				} else {
					remaining[cave.name] = 1
				}
			} else {
				remaining[cave.name] = math.MaxInt
			}
		}
		remaining["start"] = 0
		s := g.dfs2("start", remaining, "start")
		for _, t := range s {
			res[t] = struct{}{}
		}
	}

	return len(res)
}

func (g Graph) dfs2(cur string, remaining map[string]int, s string) []string {
	if cur == "end" {
		return []string{s}
	}

	edges := g.grid[cur]
	var res []string
	for _, edge := range edges {
		if remaining[edge] == 0 {
			continue
		}

		remaining[edge]--
		res = append(res, g.dfs2(edge, remaining, s+","+edge)...)
		remaining[edge]++
	}
	return res
}

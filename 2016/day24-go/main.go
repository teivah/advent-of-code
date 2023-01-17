package main

import (
	"bufio"
	"fmt"
	"io"
	"math"
)

func fs1(input io.Reader) (int, error) {
	scanner := bufio.NewScanner(input)

	var grid [][]Unit
	locations := make(map[int]Position)
	maxLocation := 0
	row := -1
	for scanner.Scan() {
		row++
		s := scanner.Text()
		line := make([]Unit, len(s))
		for i := 0; i < len(s); i++ {
			if s[i] == '#' {
				line[i] = Unit{wall: true}
			} else if s[i] == '.' {
				line[i] = Unit{empty: true}
			} else if s[i] >= '0' && s[i] <= '9' {
				location := int(s[i] - '0')
				maxLocation = max(maxLocation, location)
				locations[location] = Position{
					row: row,
					col: i,
				}
				line[i] = Unit{location: location}
			} else {
				panic(s)
			}
		}
		grid = append(grid, line)
	}

	paths := computeShortestPaths(grid, maxLocation, locations)

	var nodes []int
	for i := 0; i <= maxLocation; i++ {
		nodes = append(nodes, i)
	}
	perms := permutations(1, nodes)

	best := math.MaxInt
	for _, perm := range perms {
		sum := 0

		for i := 0; i < len(perm)-1; i++ {
			a := perm[i]
			b := perm[i+1]
			distance := paths[Pair{a, b}]
			sum += distance
		}

		best = min(best, sum)
	}

	return best, nil
}

func permutations(i int, cur []int) [][]int {
	if i == len(cur) {
		res := make([]int, len(cur))
		copy(res, cur)
		return [][]int{res}
	}

	var res [][]int
	for j := i; j < len(cur); j++ {
		cur[i], cur[j] = cur[j], cur[i]
		res = append(res, permutations(i+1, cur)...)
		cur[i], cur[j] = cur[j], cur[i]
	}
	return res
}

type Pair struct {
	from int
	to   int
}

type Position struct {
	row int
	col int
}

func (p Position) delta(row, col int) Position {
	return Position{
		row: p.row + row,
		col: p.col + col,
	}
}

func (p Position) isValid(grid [][]Unit) bool {
	if p.row < 0 || p.col < 0 || p.row >= len(grid) || p.col >= len(grid[0]) {
		return false
	}
	return !grid[p.row][p.col].wall
}

type State struct {
	position Position
	moves    int
}

func computeShortestPaths(grid [][]Unit, maxLocation int, locations map[int]Position) map[Pair]int {
	res := make(map[Pair]int)
	pairs := getAllPairs(maxLocation)

	for _, pair := range pairs {
		a := pair[0]
		b := pair[1]

		v := bfs(a, b, locations[a], locations[b], grid)
		res[Pair{a, b}] = v
		res[Pair{b, a}] = v
	}

	return res
}

func addCache(cache map[int]map[Position]int, id int, position Position, value int) {
	v, exists := cache[id]
	if !exists {
		v = make(map[Position]int)
		cache[id] = v
	}
	v[position] = value
}

func contains(cache map[int]map[Position]int, id int, position Position) (int, bool) {
	v, exists := cache[id]
	if !exists {
		return 0, false
	}
	v2, exists := v[position]
	return v2, exists
}

func bfs(fromID, toID int, from Position, to Position, grid [][]Unit) int {
	q := []State{{position: from}}
	visited := make(map[Position]struct{})
	for len(q) != 0 {
		p := q[0]
		q = q[1:]

		if _, exists := visited[p.position]; exists {
			continue
		}

		if p.position == to {
			return p.moves
		}

		if !p.position.isValid(grid) {
			continue
		}

		visited[p.position] = struct{}{}

		q = append(q, State{
			position: p.position.delta(-1, 0),
			moves:    p.moves + 1,
		})
		q = append(q, State{
			position: p.position.delta(1, 0),
			moves:    p.moves + 1,
		})
		q = append(q, State{
			position: p.position.delta(0, -1),
			moves:    p.moves + 1,
		})
		q = append(q, State{
			position: p.position.delta(0, 1),
			moves:    p.moves + 1,
		})
	}
	return -1
}

func oppositePair(p Pair) Pair {
	return Pair{
		from: p.to,
		to:   p.from,
	}
}

// No order
func getAllPairs(maxLocation int) [][]int {
	var pairs [][]int
	for i := 0; i <= maxLocation; i++ {
		for j := i + 1; j <= maxLocation; j++ {
			pairs = append(pairs, []int{i, j})
		}
	}
	return pairs
}

func printGrid(grid [][]Unit) {
	for row := 0; row < len(grid); row++ {
		for col := 0; col < len(grid[0]); col++ {
			g := grid[row][col]
			if g.wall {
				fmt.Printf("#")
			} else if g.empty {
				fmt.Printf(".")
			} else {
				fmt.Printf("%d", g.location)
			}
		}
		fmt.Println()
	}
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

type Unit struct {
	wall     bool
	empty    bool
	location int
}

func fs2(input io.Reader) (int, error) {
	scanner := bufio.NewScanner(input)

	var grid [][]Unit
	locations := make(map[int]Position)
	maxLocation := 0
	row := -1
	for scanner.Scan() {
		row++
		s := scanner.Text()
		line := make([]Unit, len(s))
		for i := 0; i < len(s); i++ {
			if s[i] == '#' {
				line[i] = Unit{wall: true}
			} else if s[i] == '.' {
				line[i] = Unit{empty: true}
			} else if s[i] >= '0' && s[i] <= '9' {
				location := int(s[i] - '0')
				maxLocation = max(maxLocation, location)
				locations[location] = Position{
					row: row,
					col: i,
				}
				line[i] = Unit{location: location}
			} else {
				panic(s)
			}
		}
		grid = append(grid, line)
	}

	paths := computeShortestPaths(grid, maxLocation, locations)

	var nodes []int
	for i := 0; i <= maxLocation; i++ {
		nodes = append(nodes, i)
	}
	perms := permutations(1, nodes)

	best := math.MaxInt
	for _, perm := range perms {
		sum := 0
		perm = append(perm, 0)

		for i := 0; i < len(perm)-1; i++ {
			a := perm[i]
			b := perm[i+1]
			distance := paths[Pair{a, b}]
			sum += distance
		}

		best = min(best, sum)
	}

	return best, nil
}

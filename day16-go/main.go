package day15_go

import (
	"bufio"
	"fmt"
	"io"
	"strconv"
	"strings"
	"time"
)

type Valve struct {
	name         string
	rate         int
	open         bool
	listChildren []string
	children     map[string]*Valve
	distance     map[string]int
}

func floydWarshall(valves map[string]*Valve) {
	for _, valve := range valves {
		for _, child := range valveNames {
			valve.distance[child] = 10000
		}
	}

	for _, valve := range valves {
		for child := range valve.children {
			valve.distance[child] = 1
		}
	}

	for _, k := range valveNames {
		for _, i := range valveNames {
			for _, j := range valveNames {
				if valves[i].distance[j] > valves[i].distance[k]+valves[k].distance[j] {
					valves[i].distance[j] = valves[i].distance[k] + valves[k].distance[j]
				}
			}
		}
	}
}

func setDistance(valves map[string]*Valve, from, to string, distance int) {
	valves[from].distance[to] = distance
}

func fn1(input io.Reader) (int, error) {
	scanner := bufio.NewScanner(input)
	valves := make(map[string]*Valve)
	for scanner.Scan() {
		s := scanner.Text()
		valve, err := toValve(s)
		if err != nil {
			return 0, err
		}
		valves[valve.name] = &valve
		valveNames = append(valveNames, valve.name)
	}

	for _, valve := range valves {
		for _, child := range valve.listChildren {
			childValve := valves[child]
			valve.children[child] = childValve
		}
	}

	floydWarshall(valves)

	cache = make(map[int]map[int]map[int]map[int]map[string]int)
	visited = make(map[string]map[int]struct{})

	start := time.Now()
	defer func() {
		fmt.Printf("%v\n", time.Since(start))
	}()

	v := make(map[string]int)
	for _, valveNames := range valveNames {
		v[valveNames] = -1
	}

	// 2=0, 3=20, 4=40, 5=63, 10=246
	return find("AA", valves, depth, v), nil
	//return findx("", "AA", valves, depth, 0, 0, len(valves)), nil
}

var depth = 30

func key(valves map[string]*Valve) int {
	k := 0
	for i, name := range valveNames {
		v := valves[name]
		if v.open {
			k += 1 << i
		}
	}
	return k
}

var valveNames []string

// Left, Pressure, Buffer, Key, Current, Value
var cache map[int]map[int]map[int]map[int]map[string]int

func addCache(current string, valves map[string]*Valve, left int, pressure int, buffer int, best int) {
	v, exists := cache[left]
	if !exists {
		v = make(map[int]map[int]map[int]map[string]int)
		cache[left] = v
	}

	v2, exists := v[pressure]
	if !exists {
		v2 = make(map[int]map[int]map[string]int)
		v[pressure] = v2
	}

	v3, exists := v2[buffer]
	if !exists {
		v3 = make(map[int]map[string]int)
		v2[buffer] = v3
	}

	k := key(valves)
	v4, exists := v3[k]
	if !exists {
		v4 = make(map[string]int)
		v3[k] = v4
	}

	v4[current] = best
}

func getCache(current string, valves map[string]*Valve, left, pressure, buffer int) (int, bool) {
	v, exists := cache[left]
	if !exists {
		return 0, false
	}

	v2, exists := v[pressure]
	if !exists {
		return 0, false
	}

	v3, exists := v2[buffer]
	if !exists {
		return 0, false
	}

	v4, exists := v3[key(valves)]
	if !exists {
		return 0, false
	}

	v5, exists := v4[current]
	return v5, exists
}

// Current, key
var visited map[string]map[int]struct{}

func isVisited(current string, valves map[string]*Valve) bool {
	v, exists := visited[current]
	if !exists {
		return false
	}

	k := key(valves)
	_, exists = v[k]
	return exists
}

func addVisited(current string, valves map[string]*Valve) {
	v, exists := visited[current]
	if !exists {
		v = make(map[int]struct{})
		visited[current] = v
	}

	k := key(valves)
	v[k] = struct{}{}
}

func delVisited(current string, valves map[string]*Valve) {
	v, exists := visited[current]
	if !exists {
		return
	}

	k := key(valves)
	delete(v, k)
}

func remaining(v map[string]int) bool {
	for _, visited := range v {
		if visited == -1 {
			return true
		}
	}
	return false
}

func score(valves map[string]*Valve, visited map[string]int) int {
	sum := 0
	for name, valve := range valves {
		since := visited[name]
		if since == -1 {
			continue
		}

		sum += valve.rate * (depth - since - 1)
	}
	return sum
}

func find(current string, valves map[string]*Valve, left int, visited map[string]int) int {
	if left == 0 {
		return score(valves, visited)
	}

	if !remaining(visited) {
		return score(valves, visited)
	}

	valve := valves[current]
	best := 0
	if !valve.open {
		valve.open = true
		visited[current] = depth - left
		best = find(current, valves, left-1, visited)
		visited[current] = -1
		valve.open = false
	}

	for child, alreadyVisited := range visited {
		if current == child {
			continue
		}
		if alreadyVisited != -1 {
			continue
		}
		distance := valve.distance[child]
		if left <= distance {
			continue
		}
		v := find(child, valves, left-distance, visited)
		if v > best {
			best = v
		}
	}

	if best == 0 {
		return score(valves, visited)
	}
	return best
}

func findx(parent, current string, valves map[string]*Valve, left int, buffer, pressure int, closed int) int {
	if left == 0 {
		return pressure
	}

	if v, exists := getCache(current, valves, left, pressure, buffer); exists {
		return v
	}

	if isVisited(current, valves) {
		return -1
	}

	if closed == 0 {
		return pressure + findx(parent, current, valves, left-1, buffer, pressure, closed)
	}

	best := -1
	valve := valves[current]
	bestWhenOpen := false
	if !valve.open {
		valve.open = true
		best = findx("", current, valves, left-1, valve.rate, buffer+pressure, closed-1)
		bestWhenOpen = true
		valve.open = false
	}

	for child := range valve.children {
		if child == parent {
			continue
		}
		addVisited(current, valves)
		v := findx(current, child, valves, left-1, 0, buffer+pressure, closed)
		delVisited(current, valves)
		if v > best {
			best = v
			bestWhenOpen = false
		}
	}

	if bestWhenOpen {
		valve.open = true
		addCache(current, valves, left, pressure, buffer, best+pressure)
		valve.open = false
	} else {
		addCache(current, valves, left, pressure, buffer, best+pressure)
	}

	return best + pressure
}

func toValve(s string) (Valve, error) {
	name := s[6:8]

	start := 23
	end := strings.Index(s[start:], ";") + start
	rate, err := strconv.Atoi(s[start:end])
	if err != nil {
		return Valve{}, err
	}

	search := "to valves "
	start = strings.Index(s, search)
	if start == -1 {
		search = "to valve "
		start = strings.Index(s, search) + len(search)
	} else {
		start += len(search)
	}
	split := strings.Split(s[start:], ", ")
	return Valve{
		name:         name,
		rate:         rate,
		open:         false,
		listChildren: split,
		children:     make(map[string]*Valve),
		distance:     make(map[string]int),
	}, nil
}

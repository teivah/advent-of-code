package day15_go

import (
	"bufio"
	"fmt"
	"io"
	"math"
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

	v := make(map[string]bool)
	for _, valveNames := range valveNames {
		v[valveNames] = false
	}

	// 2=0, 3=20
	depth := 4
	return find("AA", valves, depth, 0, 0, v), nil
	//return findx("", "AA", valves, depth, 0, 0, len(valves)), nil
}

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

func remaining(v map[string]bool) bool {
	for _, visited := range v {
		if !visited {
			return true
		}
	}
	return false
}

func find(current string, valves map[string]*Valve, left int, buffer, pressure int, visited map[string]bool) int {
	if left == 0 {
		return pressure
	}
	if left < 0 {
		return 0
	}

	if !remaining(visited) {
		return pressure + find(current, valves, left-1, 0, buffer+pressure, visited)
	}

	valve := valves[current]
	best := 0
	if !valve.open {
		valve.open = true
		addVisited(current, valves)
		best = find(current, valves, left-1, valve.rate, buffer+pressure, visited)
		delVisited(current, valves)
		valve.open = false
	}

	for child, alreadyVisited := range visited {
		if alreadyVisited {
			continue
		}
		distance := valve.distance[child]
		if left <= distance {
			continue
		}
		v := pressure*distance + find(child, valves, left-distance, 0, buffer+pressure, visited)
		if v > best {
			best = v
		}
	}

	return pressure + best + buffer
}

func findy(current string, valves map[string]*Valve, left int, buffer, currentPressure int, visited map[string]bool) int {
	if left == 0 {
		return currentPressure
	}
	if left < 0 {
		return math.MinInt
	}

	//if v, exists := getCache(current, valves, left, pressure, buffer); exists {
	//	return v
	//}

	if !remaining(visited) {
		return currentPressure + findy(current, valves, left-1, 0, buffer+currentPressure, visited)
	}

	best := 0
	valve := valves[current]
	bestWhenOpen := false
	if !valve.open {
		valve.open = true
		visited[current] = true
		best = findy(current, valves, left-1, valve.rate, buffer+currentPressure, visited)
		valve.open = false
		visited[current] = false
		bestWhenOpen = true
	}

	for dest := range visited {
		distance := valve.distance[dest]
		if left <= distance {
			continue
		}

		visited[dest] = true
		v := currentPressure*distance + findy(dest, valves, left-distance, 0, buffer+currentPressure, visited)
		visited[dest] = false

		if v > best {
			best = v
			bestWhenOpen = false
		}
	}
	_ = bestWhenOpen

	//if bestWhenOpen {
	//	valve.open = true
	//	addCache(current, valves, left, pressure, buffer, best+pressure)
	//	valve.open = false
	//} else {
	//	addCache(current, valves, left, pressure, buffer, best+pressure)
	//}

	return best + currentPressure + buffer
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

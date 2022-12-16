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

	cache = make(map[int]map[int]map[int]map[int]map[string]int)
	visited = make(map[string]map[int]struct{})

	start := time.Now()
	defer func() {
		fmt.Printf("%v\n", time.Since(start))
	}()
	return find("", "AA", valves, 30, 0, 0, len(valves)), nil
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

func find(parent, current string, valves map[string]*Valve, left int, buffer, pressure int, closed int) int {
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
		return pressure + find(parent, current, valves, left-1, buffer, pressure, closed)
	}

	best := -1
	valve := valves[current]
	bestWhenOpen := false
	if !valve.open {
		valve.open = true
		best = find("", current, valves, left-1, valve.rate, buffer+pressure, closed-1)
		bestWhenOpen = true
		valve.open = false
	}

	for child := range valve.children {
		if child == parent {
			continue
		}
		addVisited(current, valves)
		v := find(current, child, valves, left-1, 0, buffer+pressure, closed)
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
	}, nil
}

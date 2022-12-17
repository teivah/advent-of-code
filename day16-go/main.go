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

func fn1(input io.Reader, depth int) (int, error) {
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

	// First approach, navigate in all the nodes
	//return find1("", "AA", valves, depth, 0, 0, len(valves)), nil
	// Second approach, precompute the node distance (using floyd warshall) to navigate faster
	return find2(NewKey(), "AA", valves, depth, v, 0, 0, len(valveNames)), nil
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

var cacheHuman map[int]map[int]map[int]map[int]map[string]int
var cacheElephant map[string]map[int]map[int]map[int]map[int]int

// CurrentHuman, LeftHuman, PressureHuman, BufferHuman, CurrentElephant, LeftElephant, PressureElephant, BufferElephant, Key, Value
var cacheHumanElephant map[string]map[int]map[int]map[int]map[string]map[int]map[int]map[int]map[int]int

var cacheHumanElephant2 map[string]int

func getCacheHumanElephant(currentHuman string, leftHuman int, pressureHuman int, bufferHuman int, currentElephant string, leftElephant int, pressureElephant int, bufferElephant int, key *Key) (int, bool) {
	//v, exists := cacheHumanElephant2[fmt.Sprintf("%s:%d:%d:%d:%s:%d:%d:%d:%d",
	//	currentHuman, leftHuman, pressureHuman, bufferHuman, currentElephant, leftElephant, pressureElephant, bufferElephant, key.toKey())]
	//return v, exists

	if leftHuman == 0 {
		v5, exists := cacheElephant[currentElephant]
		if !exists {
			return 0, false
		}

		v6, exists := v5[leftElephant]
		if !exists {
			return 0, false
		}

		v7, exists := v6[pressureElephant]
		if !exists {
			return 0, false
		}

		v8, exists := v7[bufferElephant]
		if !exists {
			return 0, false
		}

		v9, exists := v8[key.toKey()]
		return v9, exists
	}

	v, exists := cacheHumanElephant[currentHuman]
	if !exists {
		return 0, false
	}

	v2, exists := v[leftHuman]
	if !exists {
		return 0, false
	}

	v3, exists := v2[pressureHuman]
	if !exists {
		return 0, false
	}

	v4, exists := v3[bufferHuman]
	if !exists {
		return 0, false
	}

	v5, exists := v4[currentElephant]
	if !exists {
		return 0, false
	}

	v6, exists := v5[leftElephant]
	if !exists {
		return 0, false
	}

	v7, exists := v6[pressureElephant]
	if !exists {
		return 0, false
	}

	v8, exists := v7[bufferElephant]
	if !exists {
		return 0, false
	}

	v9, exists := v8[key.toKey()]
	return v9, exists
}

func addCacheHumanElephant(currentHuman string, leftHuman int, pressureHuman int, bufferHuman int, currentElephant string, leftElephant int, pressureElephant int, bufferElephant int, key *Key, value int) {
	//cacheHumanElephant2[fmt.Sprintf("%s:%d:%d:%d:%s:%d:%d:%d:%d",
	//	currentHuman, leftHuman, pressureHuman, bufferHuman, currentElephant, leftElephant, pressureElephant, bufferElephant, key.toKey())] = value

	if leftHuman == 0 {
		v5, exists := cacheElephant[currentElephant]
		if !exists {
			v5 = make(map[int]map[int]map[int]map[int]int)
			cacheElephant[currentElephant] = v5
		}

		v6, exists := v5[leftElephant]
		if !exists {
			v6 = make(map[int]map[int]map[int]int)
			v5[leftElephant] = v6
		}

		v7, exists := v6[pressureElephant]
		if !exists {
			v7 = make(map[int]map[int]int)
			v6[pressureElephant] = v7
		}

		v8, exists := v7[bufferElephant]
		if !exists {
			v8 = make(map[int]int)
			v7[bufferElephant] = v8
		}

		v8[key.toKey()] = value
	}

	v, exists := cacheHumanElephant[currentHuman]
	if !exists {
		v = make(map[int]map[int]map[int]map[string]map[int]map[int]map[int]map[int]int)
		cacheHumanElephant[currentHuman] = v
	}

	v2, exists := v[leftHuman]
	if !exists {
		v2 = make(map[int]map[int]map[string]map[int]map[int]map[int]map[int]int)
		v[leftHuman] = v2
	}

	v3, exists := v2[pressureHuman]
	if !exists {
		v3 = make(map[int]map[string]map[int]map[int]map[int]map[int]int)
		v2[pressureHuman] = v3
	}

	v4, exists := v3[bufferHuman]
	if !exists {
		v4 = make(map[string]map[int]map[int]map[int]map[int]int)
		v3[bufferHuman] = v4
	}

	v5, exists := v4[currentElephant]
	if !exists {
		v5 = make(map[int]map[int]map[int]map[int]int)
		v4[currentElephant] = v5
	}

	v6, exists := v5[leftElephant]
	if !exists {
		v6 = make(map[int]map[int]map[int]int)
		v5[leftElephant] = v6
	}

	v7, exists := v6[pressureElephant]
	if !exists {
		v7 = make(map[int]map[int]int)
		v6[pressureElephant] = v7
	}

	v8, exists := v7[bufferElephant]
	if !exists {
		v8 = make(map[int]int)
		v7[bufferElephant] = v8
	}

	v8[key.toKey()] = value
}

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

func addCache2(current string, key *Key, left int, pressure int, buffer int, best int) {
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

	k := key.toKey()
	v4, exists := v3[k]
	if !exists {
		v4 = make(map[string]int)
		v3[k] = v4
	}

	v4[current] = best
}

func addCacheHuman(current string, key *Key, left int, pressure int, buffer int, best int) {
	v, exists := cacheHuman[left]
	if !exists {
		v = make(map[int]map[int]map[int]map[string]int)
		cacheHuman[left] = v
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

	k := key.toKey()
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

func getCache2(current string, key *Key, left, pressure, buffer int) (int, bool) {
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

	v4, exists := v3[key.toKey()]
	if !exists {
		return 0, false
	}

	v5, exists := v4[current]
	return v5, exists
}

func getCacheHuman(current string, key *Key, left, pressure, buffer int) (int, bool) {
	v, exists := cacheHuman[left]
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

	v4, exists := v3[key.toKey()]
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

func formatKey(visited map[string]int) string {
	sb := strings.Builder{}
	for _, name := range valveNames {
		v := visited[name]
		if v != -1 {
			sb.WriteString(fmt.Sprintf("%v:%d", name, v))
		}
	}
	return sb.String()
}

type Key struct {
	data []bool
	idx  map[string]int
}

func NewKey() *Key {
	data := make([]bool, len(valveNames))

	idx := make(map[string]int)
	for i, name := range valveNames {
		idx[name] = i
	}

	return &Key{
		data: data,
		idx:  idx,
	}
}

func (k *Key) visit(name string) {
	idx := k.idx[name]
	k.data[idx] = true
}

func (k *Key) unvisit(name string) {
	idx := k.idx[name]
	k.data[idx] = false
}

func (k *Key) toKey() int {
	x := 0
	for i, ok := range k.data {
		if ok {
			x += 1 << i
		}
	}
	return x
}

func find1(parent, current string, valves map[string]*Valve, left int, buffer, pressure int, closed int) int {
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
		return pressure + find1(parent, current, valves, left-1, buffer, pressure, closed)
	}

	best := -1
	valve := valves[current]
	bestWhenOpen := false
	if !valve.open {
		valve.open = true
		best = find1("", current, valves, left-1, valve.rate, buffer+pressure, closed-1)
		bestWhenOpen = true
		valve.open = false
	}

	for child := range valve.children {
		if child == parent {
			continue
		}
		addVisited(current, valves)
		v := find1(current, child, valves, left-1, 0, buffer+pressure, closed)
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

func find2(key *Key, current string, valves map[string]*Valve, left int, visited map[string]bool, buffer, pressure, remaining int) int {
	if left == 0 {
		return pressure
	}

	if v, exists := getCache2(current, key, left, pressure, buffer); exists {
		return v
	}

	if remaining == 0 {
		return pressure + find2(key, current, valves, left-1, visited, 0, buffer+pressure, 0)
	}

	valve := valves[current]
	best := 0
	bestWhenOpen := false
	if !valve.open {
		valve.open = true
		visited[current] = true
		key.visit(current)
		best = find2(key, current, valves, left-1, visited, valve.rate, buffer+pressure, remaining-1)
		key.unvisit(current)
		visited[current] = false
		valve.open = false
		bestWhenOpen = true
	}

	v := find2(key, current, valves, left-1, visited, 0, buffer+pressure, remaining)
	if v > best {
		best = v
		bestWhenOpen = false
	}

	for child, alreadyVisited := range visited {
		if current == child || alreadyVisited || valves[child].rate == 0 {
			continue
		}
		distance := valve.distance[child]
		if left <= distance {
			continue
		}
		//v := find2(key, child, valves, left-1, visited, 0, buffer+pressure, distance-1, remaining)
		v := find2(key, child, valves, left-distance, visited, 0, buffer+pressure, remaining) + (pressure+buffer)*(distance-1)
		if v > best {
			best = v
			bestWhenOpen = false
		}
	}

	if bestWhenOpen {
		valve.open = true
		addCache2(current, key, left, pressure, buffer, best+pressure)
		valve.open = false
	} else {
		addCache2(current, key, left, pressure, buffer, best+pressure)
	}

	return pressure + best
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

func fn2(input io.Reader, depth int) (int, error) {
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

	cacheHumanElephant = make(map[string]map[int]map[int]map[int]map[string]map[int]map[int]map[int]map[int]int)
	cacheElephant = make(map[string]map[int]map[int]map[int]map[int]int)

	start := time.Now()
	defer func() {
		fmt.Printf("%v\n", time.Since(start))
	}()

	v := make(map[string]bool)
	for _, valveNames := range valveNames {
		v[valveNames] = false
	}

	return findWithElephant(NewKey(), "AA", "AA", valves, depth, depth, v, 0, 0, 0, 0, len(valveNames)), nil
}

func findWithElephant(key *Key, currentHuman, currentElephant string, valves map[string]*Valve, leftHuman, leftElephant int, visited map[string]bool, bufferHuman, pressureHuman, bufferElephant, pressureElephant, remaining int) int {
	if leftHuman == 0 && leftElephant == 0 {
		return findHuman(key, currentHuman, currentElephant, valves, leftHuman, leftElephant, visited, bufferHuman, pressureHuman, bufferElephant, pressureElephant, remaining) +
			findElephant(key, currentHuman, currentElephant, valves, leftHuman, leftElephant, visited, bufferHuman, pressureHuman, bufferElephant, pressureElephant, remaining)
	}

	if v, exists := getCacheHumanElephant(currentHuman, leftHuman, pressureHuman, bufferHuman, currentElephant, leftElephant, pressureElephant, bufferElephant, key); exists {
		return v
	}

	if leftHuman == 0 {
		v := findElephant(key, currentHuman, currentElephant, valves, leftHuman, leftElephant, visited, bufferHuman, pressureHuman, bufferElephant, pressureElephant, remaining)
		addCacheHumanElephant(currentHuman, leftHuman, pressureHuman, bufferHuman, currentElephant, leftElephant, pressureElephant, bufferElephant, key, v)
		return v
	}

	v := findHuman(key, currentHuman, currentElephant, valves, leftHuman, leftElephant, visited, bufferHuman, pressureHuman, bufferElephant, pressureElephant, remaining)
	addCacheHumanElephant(currentHuman, leftHuman, pressureHuman, bufferHuman, currentElephant, leftElephant, pressureElephant, bufferElephant, key, v)
	return v
}

func findHuman(key *Key, currentHuman, currentElephant string, valves map[string]*Valve, leftHuman, leftElephant int, visited map[string]bool, bufferHuman, pressureHuman, bufferElephant, pressureElephant int, remaining int) int {
	if leftHuman == 0 {
		return pressureHuman
	}

	if remaining == 0 {
		return pressureHuman + findWithElephant(key, currentHuman, currentElephant, valves, leftHuman-1, leftElephant, visited, 0, bufferHuman+pressureHuman, bufferElephant, pressureElephant, 0)
	}

	valve := valves[currentHuman]
	best := 0
	if !valve.open {
		valve.open = true
		visited[currentHuman] = true
		key.visit(currentHuman)
		best = findHuman(key, currentHuman, currentElephant, valves, leftHuman-1, leftElephant, visited, valve.rate, bufferHuman+pressureHuman, bufferElephant, pressureElephant, remaining-1)
		key.unvisit(currentHuman)
		visited[currentHuman] = false
		valve.open = false
	}

	v := findWithElephant(key, currentHuman, currentElephant, valves, leftHuman-1, leftElephant, visited, 0, bufferHuman+pressureHuman, bufferElephant, pressureElephant, remaining)
	if v > best {
		best = v
	}

	for child, alreadyVisited := range visited {
		if currentHuman == child || alreadyVisited || valves[child].rate == 0 {
			continue
		}
		distance := valve.distance[child]
		if leftHuman <= distance {
			continue
		}
		v := findWithElephant(key, child, currentElephant, valves, leftHuman-distance, leftElephant, visited, 0, bufferHuman+pressureHuman, bufferElephant, pressureElephant, remaining) +
			(pressureHuman+bufferHuman)*(distance-1)
		if v > best {
			best = v
		}
	}

	return pressureHuman + best
}

func findElephant(key *Key, currentHuman, currentElephant string, valves map[string]*Valve, leftHuman, leftElephant int, visited map[string]bool, bufferHuman, pressureHuman, bufferElephant, pressureElephant int, remaining int) int {
	if leftElephant == 0 {
		return pressureElephant
	}

	if remaining == 0 {
		return pressureElephant + findWithElephant(key, currentHuman, currentElephant, valves, leftHuman, leftElephant-1, visited, bufferHuman, pressureHuman, 0, bufferElephant+pressureElephant, 0)
	}

	valve := valves[currentElephant]
	best := 0
	if !valve.open {
		valve.open = true
		visited[currentElephant] = true
		key.visit(currentElephant)
		best = findElephant(key, currentHuman, currentElephant, valves, leftHuman, leftElephant-1, visited, bufferHuman, pressureHuman, valve.rate, bufferElephant+pressureElephant, remaining-1)
		key.unvisit(currentElephant)
		visited[currentElephant] = false
		valve.open = false
	}

	v := findWithElephant(key, currentHuman, currentElephant, valves, leftHuman, leftElephant-1, visited, bufferHuman, pressureHuman, 0, bufferElephant+pressureElephant, remaining)
	if v > best {
		best = v
	}

	for child, alreadyVisited := range visited {
		if currentElephant == child || alreadyVisited || valves[child].rate == 0 {
			continue
		}
		distance := valve.distance[child]
		if leftElephant <= distance {
			continue
		}
		v := findWithElephant(key, currentHuman, child, valves, leftHuman, leftElephant-distance, visited, bufferHuman, pressureHuman, 0, bufferElephant+pressureElephant, remaining) +
			(pressureElephant+bufferElephant)*(distance-1)
		if v > best {
			best = v
		}
	}

	return pressureElephant + best
}

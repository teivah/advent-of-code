package day15_go

import (
	"bufio"
	"io"
	"strconv"
	"strings"
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
	}

	for _, valve := range valves {
		for _, child := range valve.listChildren {
			childValve := valves[child]
			valve.children[child] = childValve
		}
	}

	i := find("AA", valves, 20, 0, 0, len(valves))
	return i, nil
}

func find(current string, valves map[string]*Valve, left int, buffer, pressure int, closed int) int {
	if left == 0 {
		return pressure
	}

	if closed == 0 {
		return pressure + find(current, valves, left-1, buffer, pressure, closed)
	}

	best := -1
	valve := valves[current]
	if !valve.open {
		valve.open = true
		best = find(current, valves, left-1, valve.rate, buffer+pressure, closed-1)
		valve.open = false
	}

	for child := range valve.children {
		v := find(child, valves, left-1, 0, buffer+pressure, closed)
		if v > best {
			best = v
		}
	}

	return best + pressure
}

func isVisited(parent, child string, visited map[string]struct{}) bool {
	_, exists := visited[key(parent, child)]
	return exists
}

func addVisited(parent, child string, visited map[string]struct{}) {
	visited[key(parent, child)] = struct{}{}
}

func delVisited(parent, child string, visited map[string]struct{}) {
	delete(visited, key(parent, child))
}

func key(parent, child string) string {
	return parent + ":" + child
}

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
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

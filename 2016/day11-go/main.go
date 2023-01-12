package main

import (
	"bufio"
	"fmt"
	"io"
	"math"
	"strings"
)

/*
generator + chip => shield
if chip + other generator => fried

chip: either connected to generator or with no other generator

bring to level 4

elevator: at most 2 stuff, at least 1
*/
func fs1(input io.Reader) (int, error) {
	scanner := bufio.NewScanner(input)
	var levels [][]Item
	for scanner.Scan() {
		var items []Item
		s := scanner.Text()
		if s != "-" {
			words := strings.Split(s, ",")
			for _, word := range words {
				del := strings.Index(word, "-")
				name := word[:del]
				if word[del+1] == 'g' {
					items = append(items, Item{name, true})
				} else {
					items = append(items, Item{name, false})
				}
			}
		}
		levels = append(levels, items)
	}

	cache = make(map[string]int)

	return best(0, nil, levels, 0), nil
}

func formatKey(v ...any) string {
	sb := strings.Builder{}
	for _, x := range v {
		sb.WriteString(fmt.Sprintf("%v,", x))
	}
	return sb.String()
}

var cache map[string]int

func addCache(elevator int, levels [][]Item, cur int) {
	key := formatKey(elevator, levels)
	cache[key] = cur
}

func containsCache(elevator int, levels [][]Item, cur int) bool {
	key := formatKey(elevator, levels)
	v, exists := cache[key]
	if !exists {
		return false
	}
	return v <= cur
}

func best(elevator int, elevators []Item, levels [][]Item, cur int) int {
	if len(elevators)+len(levels[0])+len(levels[1])+len(levels[2])+len(levels[3]) > 4 {
		panic(elevators)
	}

	//fmt.Println(elevator, elevators, levels, cur)
	if cur == 1000 {
		fmt.Println(cache)
		panic("")
	}

	if elevator == -1 || elevator == 4 {
		return math.MaxInt
	}

	if len(levels[0]) == 0 && len(levels[1]) == 0 && len(levels[2]) == 0 {
		return cur
	}

	if fried(levels[0]) || fried(levels[1]) || fried(levels[2]) || fried(levels[3]) {
		return math.MaxInt
	}

	if containsCache(elevator, levels, cur) {
		return math.MaxInt
	}
	addCache(elevator, levels, cur)

	min := math.MaxInt

	// Move elevator
	min = getmin(min, best(elevator+1, elevators, levels, cur+1))
	min = getmin(min, best(elevator-1, elevators, levels, cur+1))

	// Fill elevator
	if len(elevators) < 2 {
		level := levels[elevator]
		for i := 0; i < len(level); i++ {
			min = getmin(min, best(elevator, addElevator(elevators, levels[elevator][i]), removeItem(levels, elevator, i), cur))
		}
	}

	// Empty elevator
	if len(elevators) > 0 {
		levels[elevator] = append(levels[elevator], elevators...)
		min = getmin(min, best(elevator, nil, levels, cur))
	}

	return min
}

func addElevator(elevators []Item, item Item) []Item {
	res := make([]Item, 0, len(elevators)+1)
	for _, elevator := range elevators {
		res = append(res, elevator)
	}
	return append(res, item)
}

func removeItem(levels [][]Item, id int, item int) [][]Item {
	res := make([][]Item, len(levels))

	for level := 0; level < len(levels); level++ {
		if level == id {
			v := make([]Item, 0, len(levels[level])-1)
			for i := 0; i < len(levels[level]); i++ {
				if i == item {
					continue
				}
				v = append(v, levels[level][i])
			}
			res[level] = v
		} else {
			v := make([]Item, len(levels[level]))
			for i := 0; i < len(levels[level]); i++ {
				v[i] = levels[level][i]
			}
			res[level] = v
		}
	}
	return res
}

func getmin(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func fried(level []Item) bool {
	m := make(map[string]bool)
	for _, item := range level {
		_, exists := m[item.name]
		if exists {
			delete(m, item.name)
			continue
		}
		m[item.name] = item.generator
	}

	atLeastOneMicrochip := false
	atLeastOneGenerator := false
	for _, generator := range m {
		if generator {
			atLeastOneGenerator = true
		} else {
			atLeastOneMicrochip = true
		}
	}

	return atLeastOneMicrochip && atLeastOneGenerator
}

type Item struct {
	name      string
	generator bool
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

func fs2(input io.Reader) (int, error) {
	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
		line := scanner.Text()
		_ = line
	}

	return 42, nil
}

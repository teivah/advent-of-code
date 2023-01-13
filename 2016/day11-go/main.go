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
	var items []Item
	level := -1
	for scanner.Scan() {
		level++
		s := scanner.Text()
		if s != "-" {
			words := strings.Split(s, ",")
			for _, word := range words {
				del := strings.Index(word, "-")
				name := word[:del]
				if word[del+1] == 'g' {
					items = append(items, Item{
						level:     level,
						elevator:  false,
						name:      name,
						generator: true,
					})
				} else {
					items = append(items, Item{
						level:     level,
						elevator:  false,
						name:      name,
						generator: false,
					})
				}
			}
		}
	}

	cache = make(map[string]int)

	return best(0, items, 0), nil
}

func formatKey(v ...any) string {
	sb := strings.Builder{}
	for _, x := range v {
		sb.WriteString(fmt.Sprintf("%v,", x))
	}
	return sb.String()
}

var cache map[string]int

func addCache(elevator int, items []Item, cur int) {
	key := formatKey(elevator, items)
	cache[key] = cur
}

func containsCache(elevator int, items []Item, cur int) bool {
	key := formatKey(elevator, items)
	v, exists := cache[key]
	if !exists {
		return false
	}
	return cur >= v
}

func best(elevator int, items []Item, cur int) int {
	if elevator == -1 || elevator == 4 {
		return math.MaxInt
	}

	updateLevelElevatorItems(elevator, items)

	if allLastLevel(items) {
		return cur
	}

	if friedLevel(items, elevator) {
		return math.MaxInt
	}

	if containsCache(elevator, items, cur) {
		return math.MaxInt
	}

	addCache(elevator, items, cur)

	min := math.MaxInt

	// Fill elevator
	elevatorLen := lenElevator(items)
	if elevatorLen < 2 {
		for i := 0; i < len(items); i++ {
			item := items[i]
			if item.level != elevator || item.elevator {
				continue
			}
			items[i].elevator = true
			min = getmin(min, best(elevator, items, cur))
			items[i].elevator = false
		}
	}

	// Empty elevator
	if elevatorLen > 0 {
		for i := 0; i < len(items); i++ {
			item := items[i]
			if !item.elevator {
				continue
			}
			items[i].elevator = false
			items[i].level = elevator
			min = getmin(min, best(elevator, items, cur))
			items[i].elevator = true
		}
	}

	// Move elevator
	min = getmin(min, best(elevator+1, items, cur+1))
	min = getmin(min, best(elevator-1, items, cur+1))

	return min
}

func allLastLevel(items []Item) bool {
	for _, item := range items {
		if item.level != 3 {
			return false
		}
	}
	return true
}

func updateLevelElevatorItems(elevator int, items []Item) {
	for i := 0; i < len(items); i++ {
		if items[i].elevator {
			items[i].level = elevator
		}
	}
}

func lenElevator(items []Item) int {
	sum := 0
	for _, item := range items {
		if item.elevator {
			sum++
		}
	}
	return sum
}

func getmin(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func fried(level int, items []Item) bool {
	return friedLevel(items, level)
}

func friedLevel(items []Item, level int) bool {
	m := make(map[string]bool)
	for _, item := range items {
		if item.level != level {
			continue
		}

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
	level     int
	elevator  bool
	name      string
	generator bool
}

func fs2(input io.Reader) (int, error) {
	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
		line := scanner.Text()
		_ = line
	}

	return 42, nil
}

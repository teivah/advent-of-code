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
	ids = make(map[string]int)
	i := 0
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
				if _, exists := ids[name]; !exists {
					ids[name] = i
					i++
				}
			}
		}
	}

	cache = make(map[int]map[int]int)

	return best(0, items, 0), nil
}

func formatKey(elevator int, items []Item) (int, int) {
	a := 0
	b := 0
	ia := 0
	ib := 0
	for _, item := range items {
		if item.generator {
			if item.elevator {
				a += elevator << ia
			} else {
				a += item.level << (ia + 2)
			}
			ia += 4
		} else {
			if item.elevator {
				b += elevator << ib
			} else {
				b += item.level << (ib + 2)
			}
			ib += 4
		}
	}

	return a, b + elevator<<60
}

// Generator, chip
var cache map[int]map[int]int

var ids map[string]int

func addCache(elevator int, items []Item, cur int) {
	a, b := formatKey(elevator, items)
	v, exists := cache[a]
	if !exists {
		v = make(map[int]int)
		cache[a] = v
	}
	v[b] = cur
}

func containsCache(elevator int, items []Item, cur int) bool {
	a, b := formatKey(elevator, items)
	v, exists := cache[a]
	if !exists {
		return false
	}
	v2, exists := v[b]
	if !exists {
		return false
	}
	return cur >= v2
}

func print(items []Item) {
	for i := 3; i >= 0; i-- {
		fmt.Printf("%d: ", i)
		for _, item := range items {
			if item.level == i {
				fmt.Printf("%v", item.name)
				if item.generator {
					fmt.Print("-g,")
				} else {
					fmt.Print("-c,")
				}
			}
		}
		fmt.Println()
	}
	fmt.Println()
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
		//print(items)
		return math.MaxInt
	}

	if containsCache(elevator, items, cur) {
		return math.MaxInt
	}

	min := math.MaxInt
	elevatorLen := lenElevator(items)

	// Fill elevator
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

	addCache(elevator, items, cur)

	// Empty elevator
	if elevatorLen > 0 {
		// Move elevator
		min = getmin(min, best(elevator+1, items, cur+1))
		min = getmin(min, best(elevator-1, items, cur+1))

		for i := 0; i < len(items); i++ {
			if !items[i].elevator {
				continue
			}
			items[i].elevator = false
			items[i].level = elevator

			min = getmin(min, best(elevator, items, cur))
			items[i].elevator = true
		}
	}

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

package main

import (
	"bufio"
	"fmt"
	"io"
	"math"
	"strings"
)

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

	cache = make(map[int]map[int]int)

	defer func() { fmt.Println(nbcases) }()

	return best(0, items, 0, 0), nil
}

var nbcases = 0

func formatKey(elevator int, items []Item) (int, int) {
	a := 0
	b := 0
	ia := 0
	ib := 0
	for _, item := range items {
		if item.generator {
			a += item.level << ia
			if item.elevator {
				a += 1 << (ia + 2)
			}
			ia += 3
		} else {
			b += item.level << ib
			if item.elevator {
				b += 1 << (ib + 2)
			}
			ib += 3
		}
	}

	return a, b + elevator<<60
}

// Generator, chip
var cache map[int]map[int]int

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

func best(elevator int, items []Item, cur int, elevatorMoves int) int {
	if elevator < 0 || elevator == 4 {
		return math.MaxInt
	}

	updateElevatorLevel(elevator, items)

	if allLastLevel(items) {
		fmt.Println(cur)
		return cur
	}

	if friedLevel(items, elevator) {
		return math.MaxInt
	}

	if elevatorMoves >= 3 {
		return math.MaxInt
	}

	if containsCache(elevator, items, cur) {
		return math.MaxInt
	}
	addCache(elevator, items, cur)

	nbcases++

	min := math.MaxInt
	elevatorLen := lenElevator(items)

	if elevatorLen > 2 {
		return math.MaxInt
	}

	if elevatorLen == 2 {
		// Move
		min = getmin(min, best(elevator+1, items, cur+1, 0))
		updateElevatorLevel(elevator, items)

		// Empty
		for i, item := range items {
			if item.elevator {
				items[i].elevator = false
				min = getmin(min, best(elevator, items, cur, elevatorMoves+1))
				items[i].elevator = true
			}
		}

		return min
	}

	if elevatorLen == 0 {
		// Fill
		for i, item := range items {
			if item.level == elevator {
				items[i].elevator = true
				min = getmin(min, best(elevator, items, cur, elevatorMoves+1))
				items[i].elevator = false
			}
		}

		return min
	}

	if elevatorLen == 1 {
		// Empty
		for i, item := range items {
			if item.elevator {
				items[i].elevator = false
				min = getmin(min, best(elevator, items, cur, elevatorMoves+1))
				items[i].elevator = true
			}
		}
		// Fill
		for i, item := range items {
			if !item.elevator && item.level == elevator {
				items[i].elevator = true
				min = getmin(min, best(elevator, items, cur, elevatorMoves+1))
				items[i].elevator = false
			}
		}
		// Move
		min = getmin(min, best(elevator+1, items, cur+1, 0))
		updateElevatorLevel(elevator, items)
		min = getmin(min, best(elevator-1, items, cur+1, 0))
		updateElevatorLevel(elevator, items)

		return min
	}

	panic(elevatorLen)
}

func updateElevatorLevel(elevator int, items []Item) {
	for i, item := range items {
		if item.elevator {
			items[i].level = elevator
		}
	}
}

func allLastLevel(items []Item) bool {
	for _, item := range items {
		if item.level != 3 {
			return false
		}
	}
	return true
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

func friedLevel(items []Item, elevator int) bool {
	m := make(map[string]bool)
	power := false
	for _, item := range items {
		if item.level != elevator {
			continue
		}

		_, exists := m[item.name]
		if exists {
			delete(m, item.name)
			power = true
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

	if atLeastOneMicrochip && power {
		return true
	}

	return atLeastOneMicrochip && atLeastOneGenerator
}

type Item struct {
	level     int
	elevator  bool
	name      string
	generator bool
}

func (i Item) String() string {
	if i.generator {
		return i.name + "-g" + fmt.Sprintf("-%d", i.level)
	}
	return i.name + "-m" + fmt.Sprintf("-%d", i.level)
}

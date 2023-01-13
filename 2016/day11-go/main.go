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

	cache = make(map[int]map[int]int)

	defer func() { fmt.Println(nbcase) }()

	return best(0, items, 0, 0), nil
}

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

/*
Step 0
F4 .  .  .  .  .
F3 .  .  .  LG .
F2 .  HG .  .  .
F1 E  .  HM .  LM (+HM +LM)

Step 1
F4 .  .  .  .  .
F3 .  .  .  LG .
F2 E  HG HM .  LM (-HM, -LM, +HG, +HM)
F1 .  .  .  .  .

Step 2
F4 .  .  .  .  .
F3 E  HG HM LG .  (-HM, -HG, +HM, +LG, -HM, +HG) => LG, HG
F2 .  .  .  .  LM
F1 .  .  .  .  .

Step 3
F4 E  HG . LG  .  +HG
F3 .  .  HM .  .
F2 .  .  .  .  LM
F1 .  .  .  .  .

Step 4
F4 .  .  . LG  .
F3 E  HG HM .  . +HM +HG
F2 .  .  .  .  LM
F1 .  .  .  .  .

Step 5
F4 E  HG HM LG .
F3 .  .  .  .  .
F2 .  .  .  .  LM
F1 .  .  .  .  .

Step 6
F4 .  HG HM .  .
F3 E  .  .  LG .
F2 .  .  .  .  LM
F1 .  .  .  .  .

Step 7
F4 .  HG HM .  .
F3 .  .  .  .  .
F2 E  .  .  LG LM
F1 .  .  .  .  .

Step 8
F4 .  HG HM .  .
F3 E  .  .  LG LM
F2 .  .  .  .  .
F1 .  .  .  .  .

Step 9
F4 E  HG HM LG LM
F3 .  .  .  .  .
F2 .  .  .  .  .
F1 .  .  .  .  .
*/

var nbcase = 0

func best(elevator int, items []Item, cur int, elevatorMoves int) int {
	if elevator < 0 || elevator == 4 {
		return math.MaxInt
	}

	updateElevatorLevel(elevator, items)

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

	nbcase++

	min := math.MaxInt
	elevatorLen := lenElevator(items)

	if elevatorLen > 2 {
		return math.MaxInt
	}

	if elevatorLen == 2 {
		// Empty
		if elevatorMoves < 3 {
			for i, item := range items {
				if item.elevator {
					items[i].elevator = false
					min = getmin(min, best(elevator, items, cur, elevatorMoves+1))
					items[i].elevator = true
				}
			}
		}

		// Move
		min = getmin(min, best(elevator+1, items, cur+1, 0))
		updateElevatorLevel(elevator, items)
		//min = getmin(min, best(elevator-1, items, cur+1, 0))
		//updateElevatorLevel(elevator, items)

		return min
	}

	if elevatorLen == 0 {
		// Fill
		if elevatorMoves < 4 {
			for i, item := range items {
				if item.level == elevator {
					items[i].elevator = true
					min = getmin(min, best(elevator, items, cur, elevatorMoves+1))
					items[i].elevator = false
				}
			}
		}

		return min
	}

	// One element
	if elevatorMoves < 3 {
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
	}
	// Move
	min = getmin(min, best(elevator+1, items, cur+1, 0))
	updateElevatorLevel(elevator, items)
	min = getmin(min, best(elevator-1, items, cur+1, 0))
	updateElevatorLevel(elevator, items)

	return min
}

//func bestx(elevator int, items []Item, cur int) int {
//	fmt.Println(elevator, items)
//
//	if elevator == -1 || elevator == 4 {
//		return math.MaxInt
//	}
//
//	if allLastLevel(items) {
//		return cur
//	}
//
//	if friedLevel(items, elevator) {
//		//print(items)
//		return math.MaxInt
//	}
//
//	if containsCache(elevator, items, cur) {
//		return math.MaxInt
//	}
//
//	min := math.MaxInt
//	elevatorLen := lenElevator(items)
//
//	// Fill elevator
//	if elevatorLen < 2 {
//		for i := 0; i < len(items); i++ {
//			item := items[i]
//			if item.level != elevator || item.elevator {
//				continue
//			}
//			level := items[i].level
//			items[i].elevator = true
//			min = getmin(min, best(elevator, copyItems(elevator, items), cur))
//			items[i].level = level
//		}
//	}
//
//	addCache(elevator, items, cur)
//
//	if elevatorLen > 0 {
//		// Move elevator
//		min = getmin(min, best(elevator+1, copyItems(elevator+1, items), cur+1))
//		min = getmin(min, best(elevator-1, copyItems(elevator-1, items), cur+1))
//
//		// Empty elevator
//		for i := 0; i < len(items); i++ {
//			if !items[i].elevator {
//				continue
//			}
//			items[i].elevator = false
//			min = getmin(min, best(elevator, copyItems(elevator, items), cur))
//			items[i].elevator = true
//		}
//	}
//
//	return min
//}

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

//func friedLevel(items []Item, elevator int) bool {
//	level := make([]Item, 0, len(items))
//	for _, item := range items {
//		if item.level == elevator {
//			level = append(level, item)
//		}
//	}
//
//	if len(level) == 0 || len(level) == 1 {
//		return false
//	}
//
//	m := make(map[string]struct{})
//	for _, item := range level {
//		if _, exists := m[item.name]; exists {
//			delete(m, item.name)
//		} else {
//			m[item.name] = struct{}{}
//		}
//	}
//	return len(m) == 0
//}

type Item struct {
	level     int
	elevator  bool
	name      string
	generator bool
}

func action(action string, i Item) string {
	return fmt.Sprintf("%v-%s", action, i)
}

func (i Item) String() string {
	if i.generator {
		return i.name + "-g" + fmt.Sprintf("-%d", i.level)
	}
	return i.name + "-m" + fmt.Sprintf("-%d", i.level)
}

func fs2(input io.Reader) (int, error) {
	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
		line := scanner.Text()
		_ = line
	}

	return 42, nil
}

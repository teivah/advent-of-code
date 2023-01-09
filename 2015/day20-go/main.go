package main

import (
	"fmt"
)

func printDivisors(n int) {
	// loop through all the divisors of the number up to the square root of n
	for i := 1; i*i <= n; i++ {
		// check if i is a divisor of n
		if n%i == 0 {
			// if it is, print it
			fmt.Print(i, " ")
			// also print the other divisor, if it is not the same as i
			if i*i != n {
				fmt.Print(n/i, " ")
			}
		}
	}
}

func fs1(presents int) int {
	for house := 1; ; house++ {
		sum := 0

		for elf := 1; elf*elf <= house; elf++ {
			if house%elf == 0 {
				sum += elf * 10
				if elf*elf != house {
					sum += (house / elf) * 10
				}
			}
		}

		if sum > presents {
			return house
		}
	}
}

func fs2(presents, maxHouse int) int {
	elves := make(map[int]int)

	for house := 1; ; house++ {
		sum := 0

		for elf := 1; elf*elf <= house; elf++ {
			if house%elf == 0 {
				if !isFull(elves, elf) {
					sum += elf * 11
					update(elves, elf, maxHouse)
				}
				if elf*elf != house {
					elf2 := house / elf
					if !isFull(elves, elf2) {
						sum += elf2 * 11
						update(elves, elf2, maxHouse)
					}
				}
			}
		}

		if sum > presents {
			return house
		}
	}
}

func isFull(elves map[int]int, id int) bool {
	v, exists := elves[id]
	if !exists {
		return false
	}
	return v == 0
}

func update(elves map[int]int, id int, maxHouse int) {
	_, exists := elves[id]
	if !exists {
		elves[id] = maxHouse - 1
	} else {
		elves[id]--
	}
}

package main

import (
	"io"

	lib "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader, length int) int {
	numbers := lib.ReaderToInts(input)

	var window []int
	for i := 0; i < length; i++ {
		window = append(window, numbers[i])
	}

	for i := length; i < len(numbers); i++ {
		number := numbers[i]
		if !sumPossible(number, window) {
			return number
		}
		window = append(window[1:], number)
	}

	return -1
}

func sumPossible(target int, numbers []int) bool {
	for i := 0; i < len(numbers)-1; i++ {
		v := numbers[i]
		for j := i + 1; j < len(numbers); j++ {
			if v+numbers[j] == target {
				return true
			}
		}
	}
	return false
}

func fs2(input io.Reader, target int) int {
	numbers := lib.ReaderToInts(input)

	l := 0
	r := 0
	sum := 0
	var window []int
	for r < len(numbers) {
		sum += numbers[r]
		window = append(window, numbers[r])

		if sum == target {
			return lib.MaxInts(window) + lib.MinInts(window)
		}

		r++
		if sum < target {
			continue
		}

		for sum > target {
			sum -= numbers[l]
			window = window[1:]
			l++

			if sum == target {
				return lib.MaxInts(window) + lib.MinInts(window)
			}
		}
	}

	return -1
}

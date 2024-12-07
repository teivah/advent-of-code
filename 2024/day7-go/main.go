package main

import (
	"fmt"
	"io"

	"github.com/teivah/go-aoc"
)

func fs1(input io.Reader) int {
	res := 0
	for _, line := range aoc.ReaderToStrings(input) {
		target, numbers := parse(line)
		if solvable1(target, 0, numbers) {
			res += target
		}
	}
	return res
}

func solvable1(target, current int, numbers []int) bool {
	if current == 0 {
		return solvable1(target, numbers[0], numbers[1:])
	}

	if len(numbers) == 0 {
		return target == current
	}

	if current > target {
		return false
	}

	return solvable1(target, current+numbers[0], numbers[1:]) ||
		solvable1(target, current*numbers[0], numbers[1:])
}

func parse(s string) (int, []int) {
	del := aoc.NewDelimiter(s, ": ")
	target := del.GetInt(0)
	del = aoc.NewDelimiter(del.GetString(1), " ")
	return target, del.GetInts()
}

func fs2(input io.Reader) int {
	res := 0
	for _, line := range aoc.ReaderToStrings(input) {
		target, numbers := parse(line)
		if solvable2(target, 0, numbers) {
			res += target
		}
	}
	return res
}

func solvable2(target, current int, numbers []int) bool {
	if current == 0 {
		return solvable2(target, numbers[0], numbers[1:])
	}

	if len(numbers) == 0 {
		return target == current
	}

	if current > target {
		return false
	}

	return solvable2(target, current+numbers[0], numbers[1:]) ||
		solvable2(target, current*numbers[0], numbers[1:]) ||
		solvable2(target, concatenate(current, numbers[0]), numbers[1:])
}

func concatenate(a, b int) int {
	return aoc.StringToInt(fmt.Sprintf("%d%d", a, b))
}

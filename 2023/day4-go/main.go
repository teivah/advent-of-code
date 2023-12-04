package main

import (
	"bufio"
	"io"
	"math"
	"strings"

	aoc "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	res := 0

	for scanner.Scan() {
		line := scanner.Text()
		line = line[strings.Index(line, ": ")+2:]
		del := aoc.NewDelimiter(line, " | ")

		winning := make(map[int]bool)
		for _, number := range aoc.NewDelimiter(strings.TrimSpace(del.GetString(0)), " ").GetStrings() {
			number = strings.TrimSpace(number)
			if number == "" {
				continue
			}
			winning[aoc.StringToInt(number)] = true
		}

		count := 0
		for _, number := range aoc.NewDelimiter(strings.TrimSpace(del.GetString(1)), " ").GetStrings() {
			number = strings.TrimSpace(number)
			if number == "" {
				continue
			}
			if winning[aoc.StringToInt(number)] {
				count++
			}
		}
		if count == 0 {
			continue
		}
		res += int(math.Pow(2, float64(count-1)))
	}

	return res
}

func fs2(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	res := 0
	cache := make(map[int][]int)
	var copies []int
	_ = cache

	for scanner.Scan() {
		res++
		line := scanner.Text()
		idx := strings.Index(line, ": ")
		id := aoc.StringToInt(strings.TrimSpace(line[5:idx]))

		if v, exists := cache[id]; exists {
			copies = append(copies, v...)
			res += len(v)
			continue
		}

		line = line[idx+2:]
		del := aoc.NewDelimiter(line, " | ")

		winning := make(map[int]bool)
		for _, number := range aoc.NewDelimiter(strings.TrimSpace(del.GetString(0)), " ").GetStrings() {
			number = strings.TrimSpace(number)
			if number == "" {
				continue
			}
			winning[aoc.StringToInt(number)] = true
		}

		count := 0
		for _, number := range aoc.NewDelimiter(strings.TrimSpace(del.GetString(1)), " ").GetStrings() {
			number = strings.TrimSpace(number)
			if number == "" {
				continue
			}
			if winning[aoc.StringToInt(number)] {
				count++
			}
		}
		if count == 0 {
			continue
		}

		var won []int
		for i := 0; i < count; i++ {
			won = append(won, id+i+1)
		}
		cache[id] = won
		copies = append(copies, won...)
		res += len(won)
	}

	for len(copies) != 0 {
		id := copies[0]
		copies = copies[1:]

		if v, exists := cache[id]; exists {
			copies = append(copies, v...)
			res += len(v)
		}
	}

	return res
}

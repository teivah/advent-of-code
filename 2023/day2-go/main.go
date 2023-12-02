package main

import (
	"bufio"
	"io"
	"strings"

	aoc "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	const (
		maxRed   = 12
		maxGreen = 13
		maxBlue  = 14
	)

	scanner := bufio.NewScanner(input)
	possibleCount := 0
	game := 0

outer:
	for scanner.Scan() {
		game++
		line := scanner.Text()
		idx := strings.Index(line, ": ")
		line = line[idx+2:]

		del := aoc.NewDelimiter(line, "; ")
		for _, set := range del.GetStrings() {
			del2 := aoc.NewDelimiter(set, ", ")
			for _, cube := range del2.GetStrings() {
				del3 := aoc.NewDelimiter(cube, " ")
				count := del3.GetInt(0)
				color := del3.GetString(1)
				switch color {
				case "red":
					if count > maxRed {
						continue outer
					}
				case "green":
					if count > maxGreen {
						continue outer
					}
				case "blue":
					if count > maxBlue {
						continue outer
					}
				default:
					panic(color)
				}
			}
		}
		possibleCount += game
	}

	return possibleCount
}

func fs2(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	res := 0
	game := 0

	for scanner.Scan() {
		game++
		line := scanner.Text()
		idx := strings.Index(line, ": ")
		line = line[idx+2:]

		maxRed := 0
		maxGreen := 0
		maxBlue := 0

		del := aoc.NewDelimiter(line, "; ")
		for _, set := range del.GetStrings() {
			del2 := aoc.NewDelimiter(set, ", ")
			for _, cube := range del2.GetStrings() {
				del3 := aoc.NewDelimiter(cube, " ")
				count := del3.GetInt(0)
				color := del3.GetString(1)
				switch color {
				case "red":
					maxRed = max(maxRed, count)
				case "green":
					maxGreen = max(maxGreen, count)
				case "blue":
					maxBlue = max(maxBlue, count)
				default:
					panic(color)
				}
			}
		}
		res += maxRed * maxGreen * maxBlue
	}

	return res
}

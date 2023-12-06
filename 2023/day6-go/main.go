package main

import (
	"io"
	"strings"

	aoc "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	lines := aoc.ReaderToStrings(input)
	times := parse1(lines[0])
	distances := parse1(lines[1])

	res := 1
	for i := 0; i < len(times); i++ {
		res *= numberOfWays(times[i], distances[i])
	}

	return res
}

func numberOfWays(raceTime, distance int) int {
	res := 0
	for i := 1; i < raceTime; i++ {
		if travelDistance(i, raceTime) > distance {
			res++
		}
	}
	return res
}

func travelDistance(hold, raceTime int) int {
	return (raceTime - hold) * hold
}

func parse1(line string) []int {
	line = aoc.Substring(line, ": ")
	line = strings.TrimSpace(line)
	del := aoc.NewDelimiter(line, " ", aoc.WithTrimSpace())
	strings := del.GetStrings()
	var res []int
	for _, s := range strings {
		// TODO WithTrimSpace should cover this case
		if s == "" {
			continue
		}
		res = append(res, aoc.StringToInt(s))
	}
	return res
}

func fs2(input io.Reader) int {
	lines := aoc.ReaderToStrings(input)
	time := parse2(lines[0])
	distance := parse2(lines[1])

	return numberOfWays(time, distance)
}

func parse2(line string) int {
	line = aoc.Substring(line, ": ")
	line = strings.TrimSpace(line)
	del := aoc.NewDelimiter(line, " ", aoc.WithTrimSpace())
	strings := del.GetStrings()
	res := ""
	for _, s := range strings {
		// TODO WithTrimSpace should cover this case
		if s == "" {
			continue
		}
		res += s
	}
	return aoc.StringToInt(res)
}

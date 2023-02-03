package main

import (
	"fmt"
	"io"
	"strings"

	lib "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	bags := toBags(lib.ReaderToStrings(input))

	sum := 0
	for bag := range bags {
		if canContain(bags, bag, "shiny gold") {
			sum++
		}
	}

	return sum
}

func canContain(bags map[string]map[string]int, source, target string) bool {
	bag := bags[source]
	for inner := range bag {
		if inner == target {
			return true
		} else {
			if canContain(bags, inner, target) {
				return true
			}
		}
	}

	return false
}

func toBags(lines []string) map[string]map[string]int {
	bags := make(map[string]map[string]int)

	for _, line := range lines {
		line = line[:len(line)-1]
		del := lib.NewDelimiter(line, " ")
		bag := del.GetString(0) + " " + del.GetString(1)

		bags[bag] = make(map[string]int)

		if strings.Contains(line, "no other bags") {
			continue
		}

		contain := "contain "
		idx := strings.Index(line, contain) + len(contain)
		s := line[idx:]

		items := lib.NewDelimiter(s, ", ").GetStrings()
		for _, item := range items {
			words := lib.NewDelimiter(item, " ")
			bags[bag][fmt.Sprintf("%s %s", words.GetString(1), words.GetString(2))] = words.GetInt(0)
		}
	}

	return bags
}

func fs2(input io.Reader) int {
	bags := toBags(lib.ReaderToStrings(input))

	return sumInnerBags(bags, "shiny gold")
}

func sumInnerBags(bags map[string]map[string]int, source string) int {
	bag := bags[source]
	sum := 0
	for inner, count := range bag {
		sum += count
		sum += count * sumInnerBags(bags, inner)
	}

	return sum
}

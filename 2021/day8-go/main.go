package main

import (
	"bufio"
	"fmt"
	"io"
	"sort"

	aoc "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	var entries []Entry
	for scanner.Scan() {
		line := scanner.Text()
		entries = append(entries, toEntry(line))
	}

	sum := 0
	for _, entry := range entries {
		for _, digit := range entry.digits {
			switch len(digit) {
			case 2, 3, 4, 7:
				sum++
			}

		}
	}

	return sum
}

func toEntry(s string) Entry {
	del := aoc.NewDelimiter(s, " | ")
	a := del.GetString(0)
	b := del.GetString(1)
	return Entry{
		signals: aoc.NewDelimiter(a, " ").GetStrings(),
		digits:  aoc.NewDelimiter(b, " ").GetStrings(),
	}
}

type Entry struct {
	signals []string
	digits  []string
}

func fs2(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	var entries []Entry
	for scanner.Scan() {
		line := scanner.Text()
		entries = append(entries, toEntry(line))
	}

	sum := 0
	for _, entry := range entries {
		v := calc(entry)
		sum += v
	}

	return sum
}

func calc(entry Entry) int {
	found := make(map[rune]rune)
	rev := make(map[rune]rune)

	seven := ""
	one := ""
	four := ""
	eight := ""
	var sixs []string
	var fives []string
	for _, signal := range entry.signals {
		if len(signal) == 2 {
			one = signal
		} else if len(signal) == 4 {
			four = signal
		} else if len(signal) == 3 {
			seven = signal
		} else if len(signal) == 7 {
			eight = signal
		} else if len(signal) == 6 { // 0, 6, 9
			sixs = append(sixs, signal)
		} else if len(signal) == 5 { // 2, 3, 5
			fives = append(fives, signal)
		}
	}

	m := count(set(one), set(seven))
	for k, v := range m {
		if v == 1 {
			found['a'] = k
			rev[k] = 'a'
		}
	}

	m = count(set(fives[0]), set(fives[1]), set(fives[2]))
	for k, v := range m {
		if v == 1 {
			// k is either b or e
			if _, exists := set(four)[k]; exists {
				found['b'] = k
				rev[k] = 'b'
			}
		}
	}

	m = count(set(sixs[0]), set(sixs[1]), set(sixs[2]))
	for k, v := range m {
		if v == 2 {
			// k is either c, d or e
			m2 := count(set(one), map[rune]struct{}{k: {}})
			for k, v := range m2 {
				if v == 2 {
					found['c'] = k
					rev[k] = 'c'
				}
			}
		}
	}

	for k := range set(one) {
		if _, exists := rev[k]; !exists {
			found['f'] = k
			rev[k] = 'f'
		}
	}

	for k := range set(four) {
		if _, exists := rev[k]; !exists {
			found['d'] = k
			rev[k] = 'd'
		}
	}

	m = count(set(sixs[0]), set(sixs[1]), set(sixs[2]))
	for k, v := range m {
		if v == 2 {
			// k is either c, d or e
			if _, exists := rev[k]; !exists {
				found['e'] = k
				rev[k] = 'e'
			}
		}
	}

	for k := range set(eight) {
		if _, exists := rev[k]; !exists {
			found['g'] = k
			rev[k] = 'g'
		}
	}

	sum := 0
	for _, digit := range entry.digits {
		i := toDigit(rev, digit)
		sum = (sum * 10) + i
	}
	return sum
}

func toDigit(transform map[rune]rune, digit string) int {
	var res []rune
	for i := 0; i < len(digit); i++ {
		r := rune(digit[i])
		res = append(res, transform[r])
	}
	sort.Slice(res, func(i, j int) bool {
		a := res[i]
		b := res[j]
		return a < b
	})

	s := string(res)

	switch s {
	case "abcefg":
		return 0
	case "cf":
		return 1
	case "acdeg":
		return 2
	case "acdfg":
		return 3
	case "bcdf":
		return 4
	case "abdfg":
		return 5
	case "abdefg":
		return 6
	case "acf":
		return 7
	case "abcdefg":
		return 8
	case "abcdfg":
		return 9
	}

	panic(fmt.Sprintf("%s, %s", digit, s))
}

func set(s string) map[rune]struct{} {
	set := make(map[rune]struct{})
	for i := 0; i < len(s); i++ {
		set[rune(s[i])] = struct{}{}
	}
	return set
}

func count(sets ...map[rune]struct{}) map[rune]int {
	res := make(map[rune]int)

	for _, set := range sets {
		for k := range set {
			res[k]++
		}
	}

	return res
}

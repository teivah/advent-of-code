package main

import (
	"io"
	"strconv"

	lib "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	lines := lib.ReaderToStrings(input)

	groups := group(lines)
	sum := 0
	for _, group := range groups {
		if isValid(group) {
			sum++
		}
	}

	return sum
}

func isValid(s string) bool {
	remaining := map[string]bool{
		"byr": true,
		"iyr": true,
		"eyr": true,
		"hgt": true,
		"hcl": true,
		"ecl": true,
		"pid": true,
	}

	del := lib.NewDelimiter(s, " ")
	for _, field := range del.GetStrings() {
		v := field[:3]
		if remaining[v] {
			delete(remaining, v)
		}
	}

	return len(remaining) == 0
}

func group(lines []string) []string {
	i := 0
	var res []string
	s := ""
	for {
		s += lines[i]
		i++
		if i >= len(lines) {
			res = append(res, s)
			break
		}
		for ; i < len(lines); i++ {
			if lines[i] == "" {
				break
			} else {
				s += " " + lines[i]
			}
		}
		res = append(res, s)
		s = ""
		i++
		if i >= len(lines) {
			break
		}
	}
	return res
}

func fs2(input io.Reader) int {
	lines := lib.ReaderToStrings(input)

	groups := group(lines)
	sum := 0
	for _, group := range groups {
		if isValid2(group) {
			sum++
		}
	}

	return sum
}

func isValid2(s string) bool {
	remaining := map[string]bool{
		"byr": true,
		"iyr": true,
		"eyr": true,
		"hgt": true,
		"hcl": true,
		"ecl": true,
		"pid": true,
	}

	del := lib.NewDelimiter(s, " ")
	m := make(map[string]string)
	for _, field := range del.GetStrings() {
		v := field[:3]
		if remaining[v] {
			m[v] = field[4:]
			delete(remaining, v)
		}
	}

	if len(remaining) != 0 {
		return false
	}

	v := lib.StringToInt(m["byr"])
	if v < 1920 || v > 2002 {
		return false
	}

	v = lib.StringToInt(m["iyr"])
	if v < 2010 || v > 2020 {
		return false
	}

	v = lib.StringToInt(m["eyr"])
	if v < 2020 || v > 2030 {
		return false
	}

	height := m["hgt"]
	v, err := strconv.Atoi(height[:len(height)-2])
	if err != nil {
		return false
	}
	if height[len(height)-2:] == "cm" {
		if v < 150 || v > 193 {
			return false
		}
	} else if height[len(height)-2:] == "in" {
		if v < 59 || v > 76 {
			return false
		}
	} else {
		return false
	}

	hcl := m["hcl"]
	if len(hcl) != 7 {
		return false
	}
	if hcl[0] != '#' {
		return false
	}
	for i := 1; i < 7; i++ {
		r := rune(hcl[i])
		if r >= '0' && r <= '9' {
			continue
		} else if r >= 'a' && r <= 'f' {
			continue
		} else {
			return false
		}
	}

	eyes := map[string]bool{
		"amb": true,
		"blu": true,
		"brn": true,
		"gry": true,
		"grn": true,
		"hzl": true,
		"oth": true,
	}
	if !eyes[m["ecl"]] {
		return false
	}

	pid := m["pid"]
	if len(pid) != 9 {
		return false
	}
	_, err = strconv.Atoi(pid)
	if err != nil {
		return false
	}

	return true
}

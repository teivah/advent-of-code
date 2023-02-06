package main

import (
	"io"
	"regexp"
	"strings"

	aoc "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	lines := aoc.ReaderToStrings(input)

	rules := make(map[int]Rule)
	i := 0
	for ; ; i++ {
		line := lines[i]
		if line == "" {
			break
		}

		id, r := toRule(line)
		rules[id] = r
	}

	rule := formatRule(rules, 0)
	re := regexp.MustCompile(rule)

	i++
	sum := 0
	for ; i < len(lines); i++ {
		line := lines[i]

		if re.MatchString(line) {
			sum++
		}
	}

	return sum
}

func formatRule(rules map[int]Rule, ruleID int) string {
	rule := rules[ruleID]
	if rule.isLetter {
		return string(rule.letter)
	}

	var res []string
	for _, subRules := range rule.rules {
		s := ""
		for _, r := range subRules {
			v := formatRule(rules, r)
			if len(v) == 2 {
				s += v
			} else {
				s += "(" + v + ")"
			}
		}
		res = append(res, s)
	}

	if ruleID == 0 {
		return "^" + strings.Join(res, "|") + "$"
	}
	return strings.Join(res, "|")
}

func toRule(s string) (int, Rule) {
	del := aoc.NewDelimiter(s, " ")
	v := del.GetString(0)
	ruleID := aoc.StringToInt(v[:len(v)-1])

	if del.GetString(1)[0] == '"' {
		return ruleID, Rule{
			isLetter: true,
			letter:   rune(s[del.Ind[0]+2]),
		}
	}

	i := del.Ind[0] + 1
	del = aoc.NewDelimiter(s[i:], " ")
	words := del.GetStrings()
	var rules [][]int
	var current []int
	for _, word := range words {
		if word == "|" {
			rules = append(rules, current)
			current = nil
		} else {
			current = append(current, aoc.StringToInt(word))
		}
	}
	if len(current) != 0 {
		rules = append(rules, current)
	}

	return ruleID, Rule{
		rules: rules,
	}
}

type Rule struct {
	isLetter bool
	letter   rune

	rules [][]int
}

func fs2(input io.Reader) int {
	lines := aoc.ReaderToStrings(input)

	rules := make(map[int]Rule)
	i := 0
	for ; ; i++ {
		line := lines[i]
		if line == "" {
			break
		}

		id, r := toRule(line)
		rules[id] = r
	}

	longest := getSizeLongestString(lines[i:])

	rule := formatRule2(rules, 0, longest)
	re := regexp.MustCompile(rule)

	i++
	sum := 0
	for ; i < len(lines); i++ {
		line := lines[i]

		if re.MatchString(line) {
			sum++
		}
	}

	return sum
}

func getSizeLongestString(lines []string) int {
	maxer := aoc.NewMaxer()
	for _, line := range lines {
		maxer.Add(len(line))
	}
	return maxer.Get()
}

func formatRule2(rules map[int]Rule, ruleID int, remaining int) string {
	if remaining <= 0 {
		return ""
	}

	rule := rules[ruleID]
	if rule.isLetter {
		return string(rule.letter)
	}

	var res []string
	for _, subRules := range rule.rules {
		s := ""
		for _, r := range subRules {
			v := formatRule2(rules, r, remaining-1)
			if len(v) == 2 {
				s += v
			} else {
				s += "(" + v + ")"
			}
		}
		res = append(res, s)
	}

	if ruleID == 0 {
		return "^" + strings.Join(res, "|") + "$"
	}
	return strings.Join(res, "|")
}

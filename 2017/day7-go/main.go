package main

import (
	"bufio"
	lib "github.com/teivah/advent-of-code"
	"io"
	"strconv"
	"strings"
)

func fs1(input io.Reader) (string, error) {
	scanner := bufio.NewScanner(input)
	towers := make(map[string]Tower)
	for scanner.Scan() {
		s := scanner.Text()
		tower := toTower(s)
		towers[tower.name] = tower
	}

	return getBottom(towers), nil
}

func getBottom(towers map[string]Tower) string {
	inDegree := make(map[string]int)
	for name := range towers {
		inDegree[name] = 0
	}
	for _, tower := range towers {
		for _, child := range tower.children {
			inDegree[child]++
		}
	}

	for k, v := range inDegree {
		if v == 0 {
			return k
		}
	}

	return ""
}

type Tower struct {
	name     string
	weight   int
	children []string
}

func toTower(s string) Tower {
	spaces := lib.IndexAll(s, " ")
	name := s[:spaces[0]]

	v := s[strings.Index(s, "(")+1 : strings.Index(s, ")")]
	weight, err := strconv.Atoi(v)
	if err != nil {
		panic(err)
	}

	var children []string
	if sep := strings.Index(s, "->"); sep != -1 {
		s = s[sep+3:]
		children = strings.Split(s, ", ")
	}

	return Tower{
		name:     name,
		weight:   weight,
		children: children,
	}
}

func fs2(input io.Reader) (int, error) {
	scanner := bufio.NewScanner(input)
	towers := make(map[string]Tower)
	for scanner.Scan() {
		s := scanner.Text()
		tower := toTower(s)
		towers[tower.name] = tower
	}

	_, v := fix(towers, getBottom(towers))
	return v, nil
}

// Recursive weight, fix
func fix(towers map[string]Tower, current string) (int, int) {
	parent := towers[current]
	if len(parent.children) == 0 {
		return parent.weight, -1
	}

	v, res := fix(towers, parent.children[0])
	if res != -1 {
		return 0, res
	}
	sum := v
	expected := v
	got := 0
	found := false
	weigths := make(map[string]int)
	weigths[parent.children[0]] = v
	for i := 1; i < len(parent.children); i++ {
		got, v = fix(towers, parent.children[i])
		if v != -1 {
			return 0, v
		}
		if got != expected {
			found = true
		}
		sum += got
		weigths[parent.children[i]] = got
	}

	if !found {
		return sum + parent.weight, -1
	}

	wrongChildren, wrongWeight := findWrongWeight(weigths)
	good := 0
	if wrongWeight == expected {
		good = got
	} else {
		good = expected
	}

	sum += parent.weight
	total := parent.weight + 3*good

	diff := total - sum

	return 0, towers[wrongChildren].weight + diff
}

func findWrongWeight(weights map[string]int) (string, int) {
	res := make(map[int]int)
	for _, v := range weights {
		res[v]++
	}

	weight := 0
	for k, v := range res {
		if v == 1 {
			weight = k
			break
		}
	}

	for k, v := range weights {
		if v == weight {
			return k, v
		}
	}

	return "", 0
}

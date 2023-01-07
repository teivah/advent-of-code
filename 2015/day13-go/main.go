package main

import (
	"bufio"
	"io"
	"strconv"
	"strings"
)

func fs1(input io.Reader) (int, error) {
	scanner := bufio.NewScanner(input)
	wishes := make(map[string]map[string]int)
	for scanner.Scan() {
		s := scanner.Text()
		idx := indexAll(s, " ")
		from := s[:idx[0]]
		gain := true
		if s[idx[1]+1] == 'l' {
			gain = false
		}
		v, err := strconv.Atoi(s[idx[2]+1 : idx[3]])
		if err != nil {
			return 0, err
		}
		if !gain {
			v = -v
		}
		to := s[idx[9]+1 : len(s)-1]

		_, exists := wishes[from]
		if !exists {
			wishes[from] = make(map[string]int)
		}
		wishes[from][to] = v
	}

	seat := make(map[string]bool)
	for k := range wishes {
		seat[k] = false
	}

	t := Table{
		table:  make([]string, len(wishes)),
		wishes: wishes,
		seat:   seat,
	}

	return t.findBest(), nil
}

type Table struct {
	table  []string
	wishes map[string]map[string]int
	seat   map[string]bool
}

func (t *Table) findBest() int {
	allSeat := true
	nextSeat := 0
	for i, name := range t.table {
		if name == "" {
			allSeat = false
			nextSeat = i
		}
	}
	if allSeat {
		return t.sum()
	}

	best := 0
	for person, seated := range t.seat {
		if seated {
			continue
		}

		t.table[nextSeat] = person
		t.seat[person] = true
		best = max(best, t.findBest())
		t.table[nextSeat] = ""
		t.seat[person] = false
	}

	return best
}

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

func (t *Table) sum() int {
	sum := 0
	for i := 0; i < len(t.table); i++ {
		from := t.table[i]
		left := t.left(i)
		right := t.right(i)

		sum += t.wishes[from][left] + t.wishes[from][right]
	}
	return sum
}

func (t *Table) left(i int) string {
	if i == 0 {
		return t.table[len(t.table)-1]
	}
	return t.table[i-1]
}

func (t *Table) right(i int) string {
	if i == len(t.table)-1 {
		return t.table[0]
	}
	return t.table[i+1]
}

const me = "me"

func fs2(input io.Reader) (int, error) {
	scanner := bufio.NewScanner(input)
	wishes := make(map[string]map[string]int)
	persons := make(map[string]struct{})
	for scanner.Scan() {
		s := scanner.Text()
		idx := indexAll(s, " ")
		from := s[:idx[0]]
		gain := true
		if s[idx[1]+1] == 'l' {
			gain = false
		}
		v, err := strconv.Atoi(s[idx[2]+1 : idx[3]])
		if err != nil {
			return 0, err
		}
		if !gain {
			v = -v
		}
		to := s[idx[9]+1 : len(s)-1]

		_, exists := wishes[from]
		if !exists {
			wishes[from] = make(map[string]int)
		}
		wishes[from][to] = v
		persons[from] = struct{}{}
		persons[to] = struct{}{}
	}

	wishes[me] = make(map[string]int)
	for person := range persons {
		wishes[person][me] = 0
		wishes[me][person] = 0
	}

	seat := make(map[string]bool)
	for k := range wishes {
		seat[k] = false
	}

	t := Table{
		table:  make([]string, len(wishes)),
		wishes: wishes,
		seat:   seat,
	}

	return t.findBest(), nil
}

func indexAll(s string, search string) []int {
	i := 0
	var res []int
	for i < len(s) {
		index := strings.Index(s[i:], search)
		if index == -1 {
			return res
		}
		res = append(res, index+i)
		i += index + len(search)
	}
	return res
}

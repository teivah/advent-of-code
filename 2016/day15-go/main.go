package main

import (
	"bufio"
	"io"
	"strconv"
	"strings"
)

func fs1(input io.Reader) (int, error) {
	scanner := bufio.NewScanner(input)
	var discs []Disc
	for scanner.Scan() {
		s := scanner.Text()
		idx := indexAll(s, " ")
		positions, err := strconv.Atoi(s[idx[2]+1 : idx[3]])
		if err != nil {
			return 0, nil
		}
		position, err := strconv.Atoi(s[idx[10]+1 : len(s)-1])
		if err != nil {
			return 0, nil
		}
		discs = append(discs, Disc{positions, position})
	}

	for i := 0; ; i++ {
		if find(0, discs, 0, i+1) {
			return i, nil
		}
	}
}

func find(i int, discs []Disc, previousPosition int, time int) bool {
	if i == len(discs) {
		return true
	}

	position := discs[i].getPosition(time)
	if i != 0 && position != previousPosition {
		return false
	}

	return find(i+1, discs, position, time+1)
}

type Disc struct {
	positions int
	position  int
}

func (d Disc) getPosition(time int) int {
	return (d.position + time) % d.positions
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

func fs2(input io.Reader) (int, error) {
	scanner := bufio.NewScanner(input)
	var discs []Disc
	for scanner.Scan() {
		s := scanner.Text()
		idx := indexAll(s, " ")
		positions, err := strconv.Atoi(s[idx[2]+1 : idx[3]])
		if err != nil {
			return 0, nil
		}
		position, err := strconv.Atoi(s[idx[10]+1 : len(s)-1])
		if err != nil {
			return 0, nil
		}
		discs = append(discs, Disc{positions, position})
	}
	discs = append(discs, Disc{11, 0})

	for i := 0; ; i++ {
		if find(0, discs, 0, i+1) {
			return i, nil
		}
	}
}

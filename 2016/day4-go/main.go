package main

import (
	"bufio"
	"io"
	"regexp"
	"sort"
	"strconv"
	"strings"
)

var re = regexp.MustCompile(`^([a-z]+-)*(\d+)\[([a-z]+)\]$`)

func fs1(input io.Reader) (int, error) {
	scanner := bufio.NewScanner(input)
	sum := 0
	for scanner.Scan() {
		if v, valid := isValid(scanner.Text()); valid {
			sum += v
		}
	}

	return sum, nil
}

func isValid(s string) (int, bool) {
	if !re.MatchString(s) {
		return 0, false
	}

	match := re.FindStringSubmatch(s)
	i, err := strconv.Atoi(match[2])
	if err != nil {
		panic(err)
	}

	l := s[:strings.Index(s, match[2])]
	occurences := make(map[uint8]int)
	for i := 0; i < len(l); i++ {
		c := l[i]
		if c == '-' {
			continue
		}
		occurences[c]++
	}

	var letters []Letter
	for k, v := range occurences {
		letters = append(letters, Letter{
			c:         k,
			occurence: v,
		})
	}

	sort.Slice(letters, func(i, j int) bool {
		a := letters[i]
		b := letters[j]
		return a.occurence > b.occurence
	})

	checksum := match[3]
	idx := 0
	for i := 0; i < len(checksum); i++ {
		c := checksum[i]
		if letters[idx].occurence != occurences[c] {
			return 0, false
		}
		idx++
	}

	return i, true
}

type Letter struct {
	c         uint8
	occurence int
}

func fs2(input io.Reader) (int, error) {
	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
		if i := decrypt(scanner.Text()); i != -1 {
			return i, nil
		}
	}

	return -1, nil
}

func decrypt(s string) int {
	if !re.MatchString(s) {
		return -1
	}

	match := re.FindStringSubmatch(s)
	i, err := strconv.Atoi(match[2])
	if err != nil {
		panic(err)
	}

	l := s[:strings.Index(s, match[2])]

	runes := []rune(l)
	var x = uint8(i % 26)
	for i := 0; i < len(runes); i++ {
		r := uint8(runes[i])
		r = ((r-'a')+x)%26 + 'a'
		runes[i] = rune(r)
	}

	if strings.Index(string(runes), "northpole") != -1 {
		return i
	}

	return -1
}

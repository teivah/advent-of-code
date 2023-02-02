package main

import (
	"bufio"
	"io"
	"strings"

	lib "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	sum := 0
	for scanner.Scan() {
		pol := toPolicy(scanner.Text())

		count := 0
		for i := 0; i < len(pol.s); i++ {
			r := rune(pol.s[i])
			if r == pol.letter {
				count++
			}
		}

		if count >= pol.from && count <= pol.to {
			sum++
		}
	}

	return sum
}

type Policy struct {
	from   int
	to     int
	letter rune
	s      string
}

func toPolicy(s string) Policy {
	del := lib.NewDelimiter(s, " ")
	first := del.GetString(0)
	id := strings.Index(first, "-")
	return Policy{
		from:   lib.StringToInt(first[:id]),
		to:     lib.StringToInt(first[id+1:]),
		letter: rune(s[del.Ind[0]+1]),
		s:      del.GetString(2),
	}
}

func fs2(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	sum := 0
	for scanner.Scan() {
		pol := toPolicy(scanner.Text())

		pol.from--
		pol.to--

		count := 0
		if pol.from < len(pol.s) && rune(pol.s[pol.from]) == pol.letter {
			count++
		}
		if pol.to < len(pol.s) && rune(pol.s[pol.to]) == pol.letter {
			count++
		}

		if count == 1 {
			sum++
		}
	}

	return sum
}

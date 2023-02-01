package main

import (
	"fmt"
	"io"
	"strings"

	lib "github.com/teivah/advent-of-code"
)

const cards = 10007

func fs1(input io.Reader, nb, n int) int {
	lines := lib.ReaderToStrings(input)

	var funcs []apply
	for _, line := range lines {
		del := lib.NewDelimiter(line, " ")
		if strings.Contains(line, "deal with") {
			funcs = append(funcs, increment(del.GetInt(3)))
		} else if strings.Contains(line, "cut") {
			funcs = append(funcs, cut(del.GetInt(1)))
		} else if strings.Contains(line, "deal") {
			funcs = append(funcs, deal())
		} else {
			panic(line)
		}
	}

	cards := make([]int, nb)
	for i := 0; i < nb; i++ {
		cards[i] = i
	}

	for _, f := range funcs {
		cards = f(cards)
	}

	for i, card := range cards {
		if card == n {
			return i
		}
	}
	return -1
}

type apply func([]int) []int

func deal() apply {
	return func(s []int) []int {
		l := 0
		r := len(s) - 1
		for l < r {
			s[l], s[r] = s[r], s[l]
			l++
			r--
		}
		return s
	}
}

func increment(n int) apply {
	return func(s []int) []int {
		res := make([]int, len(s))
		res[0] = s[0]
		count := 1
		idx := 0
		for {
			idx = (idx + n) % len(s)
			res[idx] = s[count]
			count++
			if count == len(s) {
				break
			}
		}
		return res
	}
}

func cut(n int) apply {
	return func(s []int) []int {
		v := 0
		if n == 0 {
			return s
		}
		if n >= 0 {
			v = len(s) - n
		} else {
			v = -n
		}

		deal()(s)
		deal()(s[:v])
		deal()(s[v:])
		return s
	}
}

func fs2(input io.Reader, spaceCards, shuffling, n int) int {
	lines := lib.ReaderToStrings(input)

	var funcs []apply
	for _, line := range lines {
		del := lib.NewDelimiter(line, " ")
		if strings.Contains(line, "deal with") {
			funcs = append(funcs, increment(del.GetInt(3)))
		} else if strings.Contains(line, "cut") {
			funcs = append(funcs, cut(del.GetInt(1)))
		} else if strings.Contains(line, "deal") {
			funcs = append(funcs, deal())
		} else {
			panic(line)
		}
	}

	cards := make([]int, spaceCards)
	for i := 0; i < spaceCards; i++ {
		cards[i] = i
	}

	for shuffle := 0; shuffle < shuffling; shuffle++ {
		fmt.Println(cards)
		for _, f := range funcs {
			cards = f(cards)
		}
	}

	return cards[n]
}

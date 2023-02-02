package main

import (
	"io"
	"math/big"
	"strings"

	lib "github.com/teivah/advent-of-code"
)

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

func fs2(input io.Reader, l, times, index int64) int64 {
	v := big.NewInt(index)
	m := big.NewInt(l)
	t := big.NewInt(times)

	i := big.NewInt(0)
	d := big.NewInt(1)

	lines := lib.ReaderToStrings(input)
	for _, line := range lines {
		del := lib.NewDelimiter(line, " ")
		if strings.Contains(line, "deal with") {
			x := big.NewInt(int64(del.GetInt(3)))
			x.ModInverse(x, m)
			d.Mul(d, x)
		} else if strings.Contains(line, "cut") {
			x := big.NewInt(int64(del.GetInt(1)))
			i.Add(i, x.Mul(x, d))
		} else if strings.Contains(line, "deal") {
			d.Neg(d)
			i.Add(i, d)
		} else {
			panic(line)
		}
	}

	a := big.NewInt(1)
	a.Sub(a, d).ModInverse(a, m)

	d.Exp(d, t, m)
	it := big.NewInt(1)
	it.Sub(it, d).Mul(it, a).Mul(it, i)
	v.Mul(v, d).Add(v, it).Mod(v, m)

	return v.Int64()
}

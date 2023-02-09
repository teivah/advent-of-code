package main

import (
	"bufio"
	"io"
	"strconv"

	aoc "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
		line := scanner.Text()
		pair, _ := parse(line, 1)
		_ = pair
	}

	return 42
}

func parse(s string, i int) (*Pair, int) {
	if s[i] == '[' {
		pair1, j := parse(s, i+1)
		i = j + 1

		if s[i] == '[' {
			pair2, j := parse(s, i+1)
			return &Pair{
				x: Element{
					pair: pair1,
				},
				y: Element{
					pair: pair2,
				},
			}, j
		} else {
			return &Pair{
				x: Element{
					pair: pair1,
				},
				y: Element{
					number: aoc.RuneToInt(rune(s[i])),
				},
			}, i + 2 // Skip ]
		}
	} else {
		number1 := aoc.RuneToInt(rune(s[i]))
		i += 2 // Skip ,

		if s[i] == '[' {
			pair, j := parse(s, i+1)
			return &Pair{
				x: Element{
					number: number1,
				},
				y: Element{
					pair: pair,
				},
			}, j
		} else {
			return &Pair{
				x: Element{
					number: number1,
				},
				y: Element{
					number: aoc.RuneToInt(rune(s[i])),
				},
			}, i + 2 // Skip ]
		}
	}
}

type Element struct {
	number int
	pair   *Pair
}

type Pair struct {
	x Element
	y Element
}

func (e *Element) explode() {
	e.dfs(0)
}

func (e *Element) dfs(currentLevel int) (int, bool, int, bool) {
	if e.pair == nil {
		return 0, false, 0, false
	}

	if currentLevel >=4  {
		var xv int
		var xe bool
		if e.pair.x.pair == nil {
			xv = e.pair.x.number
			xe = true
		} else {
			xv,
		}

		var yv int
		var ye bool
		if e.pair.y.pair == nil {
			yv = e.pair.y.number
			ye = true
		} else {

		}


		return e.pair.x., true, e.pair.y, true
	}

	if e.pair.x.pair != nil {
		x, y, exploded := e.pair.x.dfs(currentLevel + 1)
		if exploded {
			if e.number {

			}
		}
	}
}

func (p *Pair) String() string {
	s := "["
	if p.x.pair == nil {
		s += strconv.Itoa(p.x.number)
	} else {
		s += p.x.pair.String()
	}
	s += ","
	if p.y.pair == nil {
		s += strconv.Itoa(p.y.number)
	} else {
		s += p.y.pair.String()
	}
	return s + "]"
}

func fs2(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
		line := scanner.Text()
		_ = line
	}

	return 42
}

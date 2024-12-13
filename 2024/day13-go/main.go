// Part 1: It sounds like a Knapsack problem, I'll go with a tabulation approach.
// Part 2: Ah sh**! OK so apparently, I may use algebra and cramer's rule (https://en.wikipedia.org/wiki/Cramer%27s_rule)
package main

import (
	"io"
	"math"

	"github.com/teivah/go-aoc"
)

type Machine struct {
	buttonA Info
	buttonB Info
	prize   Info
}

type Info struct {
	x    int
	y    int
	cost int
}

func fs1(input io.Reader) int {
	groups := aoc.StringGroups(aoc.ReaderToStrings(input))
	res := 0
	for _, lines := range groups {
		res += solve(parseMachine(lines, 0))
	}
	return res
}

func solve(machine Machine) int {
	if machine.buttonA.x*100+machine.buttonB.x*100 < machine.prize.x ||
		machine.buttonA.y*100+machine.buttonB.y*100 < machine.prize.y {
		return 0
	}

	tab := make([][]int, machine.prize.x+1)
	for x := 0; x <= machine.prize.x; x++ {
		tab[x] = make([]int, machine.prize.y+1)
	}

	for x := 0; x <= machine.prize.x; x++ {
		for y := 0; y <= machine.prize.y; y++ {
			if x == machine.buttonA.x && y == machine.buttonA.y {
				// We just need to press button A once
				tab[x][y] = machine.buttonA.cost
				continue
			}
			if x == machine.buttonB.x && y == machine.buttonB.y {
				// We just need to press button B once
				tab[x][y] = machine.buttonB.cost
				continue
			}

			best := math.MaxInt
			if v := pressButton(tab, x, y, machine.buttonA); v != 0 {
				best = min(best, v+machine.buttonA.cost)
			}
			if v := pressButton(tab, x, y, machine.buttonB); v != 0 {
				best = min(best, v+machine.buttonB.cost)
			}
			if best != math.MaxInt {
				tab[x][y] = best
			}
		}
	}

	return tab[machine.prize.x][machine.prize.y]
}

func pressButton(tab [][]int, x, y int, button Info) int {
	dx := x - button.x
	dy := y - button.y
	if dx < 0 || dy < 0 {
		return 0
	}
	return tab[dx][dy]
}

func parseMachine(lines []string, additionner int) Machine {
	return Machine{
		buttonA: parseButton(lines[0], 3),
		buttonB: parseButton(lines[1], 1),
		prize:   parsePrize(lines[2], additionner),
	}
}

func parseButton(line string, cost int) Info {
	line = line[12:]
	del := aoc.NewDelimiter(line, ", Y+")
	return Info{
		x:    del.GetInt(0),
		y:    del.GetInt(1),
		cost: cost,
	}
}

func parsePrize(line string, additionner int) Info {
	line = line[9:]
	del := aoc.NewDelimiter(line, ", Y=")
	return Info{
		x: del.GetInt(0) + additionner,
		y: del.GetInt(1) + additionner,
	}
}

func fs2(input io.Reader, additionner int) int {
	groups := aoc.StringGroups(aoc.ReaderToStrings(input))
	res := 0
	for _, lines := range groups {
		machine := parseMachine(lines, additionner)
		ax := machine.buttonA.x
		ay := machine.buttonA.y
		bx := machine.buttonB.x
		by := machine.buttonB.y
		px := machine.prize.x
		py := machine.prize.y

		d := ax*by - ay*bx
		dx := px*by - py*bx
		dy := ax*py - ay*px
		if dx%d != 0 || dy%d != 0 {
			continue
		}
		res += (dx/d)*machine.buttonA.cost + (dy/d)*machine.buttonB.cost
	}
	return res
}

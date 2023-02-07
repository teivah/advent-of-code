package main

import (
	"bufio"
	"io"

	aoc "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	var lines []Line
	for scanner.Scan() {
		line := scanner.Text()
		lines = append(lines, toLine(line))
	}

	r := aoc.NewMinerMaxer()
	c := aoc.NewMinerMaxer()
	for _, line := range lines {
		r.Add(line.from.Row, line.to.Row)
		c.Add(line.from.Col, line.to.Col)
	}

	grid := make(map[aoc.Position]int)

	for _, line := range lines {
		deltaRow := 0
		deltaCol := 0
		switch line.direction {
		case aoc.Up:
			deltaRow = -1
		case aoc.Down:
			deltaRow = 1
		case aoc.Left:
			deltaCol = -1
		case aoc.Right:
			deltaCol = 1
		case aoc.UpLeft:
			continue
		case aoc.UpRight:
			continue
		case aoc.DownLeft:
			continue
		case aoc.DownRight:
			continue
		}

		grid[line.from]++

		cur := line.from
		for cur != line.to {
			cur = cur.Delta(deltaRow, deltaCol)
			grid[cur]++
		}
	}

	sum := 0
	for _, v := range grid {
		if v >= 2 {
			sum++
		}
	}

	return sum
}

func toLine(s string) Line {
	del := aoc.NewDelimiter(s, " ")

	f := del.GetString(0)
	delF := aoc.NewDelimiter(f, ",")
	from := aoc.Position{
		Col: delF.GetInt(0),
		Row: delF.GetInt(1),
	}

	t := del.GetString(2)
	delT := aoc.NewDelimiter(t, ",")
	to := aoc.Position{
		Col: delT.GetInt(0),
		Row: delT.GetInt(1),
	}

	var direction aoc.Direction
	if from == to {

	} else if from.Row == to.Row {
		if from.Col < to.Col {
			direction = aoc.Right
		} else {
			direction = aoc.Left
		}
	} else if from.Col == to.Col {
		if from.Row < to.Row {
			direction = aoc.Down
		} else {
			direction = aoc.Up
		}
	} else {
		if from.Col < to.Col && from.Row < to.Row {
			direction = aoc.DownRight
		} else if from.Col < to.Col && from.Row > to.Row {
			direction = aoc.UpRight
		} else if from.Col > to.Col && from.Row < to.Row {
			direction = aoc.DownLeft
		} else if from.Col > to.Col && from.Row > to.Row {
			direction = aoc.UpLeft
		}
	}

	return Line{
		from:      from,
		to:        to,
		direction: direction,
	}
}

type Line struct {
	from      aoc.Position
	to        aoc.Position
	direction aoc.Direction
}

func fs2(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	var lines []Line
	for scanner.Scan() {
		line := scanner.Text()
		lines = append(lines, toLine(line))
	}

	r := aoc.NewMinerMaxer()
	c := aoc.NewMinerMaxer()
	for _, line := range lines {
		r.Add(line.from.Row, line.to.Row)
		c.Add(line.from.Col, line.to.Col)
	}

	grid := make(map[aoc.Position]int)

	for _, line := range lines {
		deltaRow := 0
		deltaCol := 0
		switch line.direction {
		case aoc.Up:
			deltaRow = -1
		case aoc.Down:
			deltaRow = 1
		case aoc.Left:
			deltaCol = -1
		case aoc.Right:
			deltaCol = 1
		case aoc.UpLeft:
			deltaRow = -1
			deltaCol = -1
		case aoc.UpRight:
			deltaRow = -1
			deltaCol = 1
		case aoc.DownLeft:
			deltaRow = 1
			deltaCol = -1
		case aoc.DownRight:
			deltaRow = 1
			deltaCol = 1
		}

		grid[line.from]++

		cur := line.from
		for cur != line.to {
			cur = cur.Delta(deltaRow, deltaCol)
			grid[cur]++
		}
	}

	sum := 0
	for _, v := range grid {
		if v >= 2 {
			sum++
		}
	}

	return sum
}

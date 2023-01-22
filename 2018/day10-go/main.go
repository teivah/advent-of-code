package main

import (
	"bufio"
	"fmt"
	"io"
	"io/ioutil"
	"log"
	"os"
	"strings"

	lib "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader, rounds int) {
	scanner := bufio.NewScanner(input)
	var points []*Point
	for scanner.Scan() {
		point := toPoint(scanner.Text())
		points = append(points, &point)
	}

	f, err := ioutil.TempFile(os.TempDir(), "prefix-")
	if err != nil {
		log.Fatal("Cannot create temporary file", err)
	}

	fmt.Println(f.Name())

	defer func() {
		if err := f.Close(); err != nil {
			panic(err)
		}
	}()

	for round := 0; round < rounds; round++ {
		for _, point := range points {
			point.move()
		}

		cols := make(map[int]int)
		for _, point := range points {
			cols[point.position.col]++
		}

		countBars := 0
		for _, v := range cols {
			if v >= 8 {
				countBars++
			}
		}
		if countBars >= 3 {
			//printBoard(toBoard(points))
			writeBoard(f, round, toBoard(points))
		}
	}
}

func writeBoard(f *os.File, round int, board [][]bool) {
	sb := strings.Builder{}
	sb.WriteString(fmt.Sprintf("round %d:\n", round))
	for _, row := range board {
		for _, v := range row {
			if v {
				sb.WriteRune('#')
			} else {
				sb.WriteRune('.')
			}
		}
		sb.WriteRune('\n')
	}
	sb.WriteRune('\n')
	sb.WriteRune('\n')
	_, err := f.WriteString(sb.String())
	if err != nil {
		panic(err)
	}
}

func printBoard(board [][]bool) {
	for _, row := range board {
		for _, v := range row {
			if v {
				fmt.Print("#")
			} else {
				fmt.Print(".")
			}
		}
		fmt.Println()
	}
	fmt.Println()
}

func toBoard(points []*Point) [][]bool {
	minerRow := lib.NewMiner()
	minerCol := lib.NewMiner()
	maxerRow := lib.NewMaxer()
	maxerCol := lib.NewMaxer()
	for _, p := range points {
		minerRow.Add(p.position.row)
		maxerRow.Add(p.position.row)
		minerCol.Add(p.position.col)
		maxerCol.Add(p.position.col)
	}
	minRow := minerRow.Get()
	minCol := minerCol.Get()
	maxRow := maxerRow.Get()
	maxCol := maxerCol.Get()

	grid := make([][]bool, maxRow-minRow+1)
	for row := minRow; row <= maxRow; row++ {
		grid[row-minRow] = make([]bool, maxCol-minCol+1)
	}

	for _, point := range points {
		grid[point.position.row-minRow][point.position.col-minCol] = true
	}
	return grid
}

type Point struct {
	position Coord
	velocity Coord
}

func (p *Point) move() {
	p.position.row += p.velocity.row
	p.position.col += p.velocity.col
}

func toPoint(s string) Point {
	start := lib.IndexAll(s, "<")
	end := lib.IndexAll(s, ">")
	comma := lib.IndexAll(s, ",")

	positionCol := lib.StringToInt(strings.Trim(s[start[0]+1:comma[0]], " "))
	positionRow := lib.StringToInt(strings.Trim(s[comma[0]+2:end[0]], " "))
	velocityCol := lib.StringToInt(strings.Trim(s[start[1]+1:comma[1]], " "))
	velocityRow := lib.StringToInt(strings.Trim(s[comma[1]+2:end[1]], " "))
	return Point{
		position: Coord{
			row: positionRow,
			col: positionCol,
		},
		velocity: Coord{
			row: velocityRow,
			col: velocityCol,
		},
	}
}

type Coord struct {
	row int
	col int
}

func fs2(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
		line := scanner.Text()
		_ = line
	}

	return 42
}

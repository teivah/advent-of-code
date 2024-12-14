package main

import (
	"fmt"
	"io"
	"math"
	"os"
	"time"

	"github.com/teivah/go-aoc"
)

func fs1(input io.Reader, seconds, rows, cols int) int {
	var robots []*Robot
	for _, line := range aoc.ReaderToStrings(input) {
		robots = append(robots, parseRobot(line))
	}
	for i := 0; i < seconds; i++ {
		for _, robot := range robots {
			robot.move(rows, cols)
		}
	}

	positions := make(map[aoc.Position][]*Robot)
	for _, robot := range robots {
		positions[robot.pos] = append(positions[robot.pos], robot)
	}

	res := 1
	res *= countQuadrant(positions, 0, rows/2, 0, cols/2)
	res *= countQuadrant(positions, rows/2+1, rows, 0, cols/2)
	res *= countQuadrant(positions, 0, rows/2, cols/2+1, cols)
	res *= countQuadrant(positions, rows/2+1, rows, cols/2+1, cols)
	return res
}

func countQuadrant(positions map[aoc.Position][]*Robot, fromRow, toRow, fromCol, toCol int) int {
	count := 0
	for row := fromRow; row < toRow; row++ {
		for col := fromCol; col < toCol; col++ {
			count += len(positions[aoc.NewPosition(row, col)])
		}
	}
	return count
}

type Robot struct {
	pos  aoc.Position
	vrow int
	vcol int
}

func (r *Robot) move(rows, cols int) {
	r.pos = aoc.NewPosition(aoc.Mod(r.pos.Row+r.vrow, rows), aoc.Mod(r.pos.Col+r.vcol, cols))
}

func parseRobot(line string) *Robot {
	del := aoc.NewDelimiter(line, " ")
	del2 := aoc.NewDelimiter(del.GetString(0)[2:], ",")
	del3 := aoc.NewDelimiter(del.GetString(1)[2:], ",")
	pos := aoc.NewPosition(del2.GetInt(1), del2.GetInt(0))
	return &Robot{
		pos:  pos,
		vrow: del3.GetInt(1),
		vcol: del3.GetInt(0),
	}
}

func fs2(input io.Reader, seconds, rows, cols int) {
	var robots []*Robot
	for _, line := range aoc.ReaderToStrings(input) {
		robots = append(robots, parseRobot(line))
	}
	for i := 0; i < seconds; i++ {
		for _, robot := range robots {
			robot.move(rows, cols)
		}
		// 360
		if i > 0 {
			if !display(robots, rows, cols) {
				continue
			}
			fmt.Println(i + 1)
			time.Sleep(200 * time.Millisecond)
		}
	}
}

func display(robots []*Robot, rows, cols int) bool {
	positions := make(map[aoc.Position][]*Robot)
	for _, robot := range robots {
		positions[robot.pos] = append(positions[robot.pos], robot)
	}

	found := false

outer:
	for col := 0; col < cols; col++ {
		for row := 0; row < rows; row++ {
			pos := aoc.NewPosition(row, col)
			if len(positions[pos]) > 0 {
				row++
				count := 1
				for ; row < rows; row++ {
					pos := aoc.NewPosition(row, col)
					if len(positions[pos]) > 0 {
						count++
						if count >= 5 {
							found = true
							break outer
						}
						continue
					} else {
						break
					}
				}
			}
		}
	}

	if !found {
		return false
	}

	for row := 0; row < rows; row++ {
		for col := 0; col < cols; col++ {
			pos := aoc.NewPosition(row, col)
			if len(positions[pos]) == 0 {
				fmt.Print(" ")
			} else {
				fmt.Print("#")
			}
		}
		fmt.Println()
	}
	return true
}

func main() {
	f, _ := os.Open("input.txt")
	fs2(f, math.MaxInt, 103, 101)
}

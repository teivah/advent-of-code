package main

import (
	"bufio"
	"fmt"
	"io"
	"math"
	"strings"

	lib "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	var grid [][]bool
	nRow := -1
	asteroids := make(map[lib.Position]bool)
	for scanner.Scan() {
		nRow++
		line := strings.TrimSpace(scanner.Text())
		var row []bool
		for col := 0; col < len(line); col++ {
			v := line[col]
			if v == '#' {
				row = append(row, true)
				asteroids[lib.Position{nRow, col}] = true
			} else {
				row = append(row, false)
			}
		}
		grid = append(grid, row)
	}

	_, v := getMonitoringStation(grid, asteroids)
	return v
}

func getMonitoringStation(grid [][]bool, asteroids map[lib.Position]bool) (lib.Position, int) {
	max := 0
	var best lib.Position
	for position := range asteroids {
		v := countVisibleAsteroinds(grid, position, asteroids)
		if v > max {
			max = v
			best = position
		}
	}
	return best, max
}

func countVisibleAsteroinds(grid [][]bool, from lib.Position, asteroids map[lib.Position]bool) int {
	sum := 0
	for asteroid := range asteroids {
		if canDetect(grid, from, asteroid) {
			sum++
		}
	}
	return sum
}

func canDetect(grid [][]bool, from, to lib.Position) bool {
	if from == to {
		return false
	}

	distanceRow := to.Row - from.Row
	distanceCol := to.Col - from.Col

	distance := from.Manhattan(to)
	deltaRow := float64(distanceRow) / float64(distance)
	deltaCol := float64(distanceCol) / float64(distance)
	row := float64(from.Row) + deltaRow
	col := float64(from.Col) + deltaCol

	for i := 0; i < distance; i++ {
		if isInt(row) && isInt(col) {
			if int(math.Round(row)) == to.Row && int(math.Round(col)) == to.Col {
				return true
			}
			if grid[int(math.Round(row))][int(math.Round(col))] {
				return false
			}
		}

		row += deltaRow
		col += deltaCol
	}
	panic(fmt.Sprintf("%v, %v", from, to))
}

func isInt(f float64) bool {
	v := f - float64(int(f))
	return v < 0.000001 || v > 0.99999
}

func fs2(input io.Reader, nth int) int {
	scanner := bufio.NewScanner(input)
	var grid [][]bool
	nRow := -1
	asteroids := make(map[lib.Position]bool)
	for scanner.Scan() {
		nRow++
		line := strings.TrimSpace(scanner.Text())
		var row []bool
		for col := 0; col < len(line); col++ {
			v := line[col]
			if v == '#' {
				row = append(row, true)
				asteroids[lib.Position{nRow, col}] = true
			} else {
				row = append(row, false)
			}
		}
		grid = append(grid, row)
	}

	station, _ := getMonitoringStation(grid, asteroids)

	currentAngle := 90.
	for {
		var options []lib.Position
		for asteroid := range asteroids {
			if canDetect(grid, station, asteroid) {
				options = append(options, asteroid)
			}
		}
		angles := make([]float64, 0, len(options))
		for _, option := range options {
			angles = append(angles, calcAngle(station, option))
		}

		// Which angle is the closest from currentAngle but smaller
		bestAngleIndex := -1
		bestDistance := 361.
		for i, angle := range angles {
			distance := currentAngle - angle
			if distance < 0 {
				continue
			}
			if distance < bestDistance {
				bestDistance = distance
				bestAngleIndex = i
			}
		}

		if bestAngleIndex == -1 {
			// We need to find the max angle
			max := -1.
			for i, angle := range angles {
				if angle > max {
					max = angle
					bestAngleIndex = i
				}
			}
		}

		// Shoot i
		target := options[bestAngleIndex]

		currentAngle = angles[bestAngleIndex] - 0.0001
		if currentAngle < 0 {
			currentAngle = 359.999
		}
		nth--
		if nth == 0 {
			return target.Col*100 + target.Row
		}
		delete(asteroids, target)
		grid[target.Row][target.Col] = false
	}
}

func calcAngle(a, b lib.Position) float64 {
	angleRad := math.Atan2(float64(a.Row)-float64(b.Row), float64(b.Col)-float64(a.Col))

	v := angleRad * 180 / math.Pi
	if v < 0 {
		v = 180 - v
	}

	return v
}

func equals(a, b float64) bool {
	tolerance := 0.001
	if diff := math.Abs(a - b); diff < tolerance {
		return true
	}
	return false
}

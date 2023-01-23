package main

import (
	"fmt"
	"math"
)

func fs1(serialNumber int) string {
	const (
		size = 300
		cell = 3
	)

	grid := make([][]int, size)
	for y := 0; y < size; y++ {
		grid[y] = make([]int, size)
	}

	for y := 0; y < size; y++ {
		for x := 0; x < size; x++ {
			grid[y][x] = powerLevel(x+1, y+1, serialNumber)
		}
	}

	max := math.MinInt
	bestX := 0
	bestY := 0
	for y := 0; y < size-cell-1; y++ {
		for x := 0; x < size-cell-1; x++ {
			sum := 0
			for row := 0; row < cell; row++ {
				for col := 0; col < cell; col++ {
					sum += grid[y+row][x+col]
				}
			}

			if sum > max {
				max = sum
				bestX = x + 1
				bestY = y + 1
			}
		}
	}

	return fmt.Sprintf("%d,%d", bestX, bestY)
}

func powerLevel(x, y, serialNumber int) int {
	rackID := x + 10
	power := (rackID*y + serialNumber) * rackID
	power = getHundredsDigit(power)
	return power - 5
}

func getHundredsDigit(i int) int {
	if i < 100 {
		return 0
	}

	return (i / 100) % 10
}

func fs2(serialNumber int) string {
	const (
		size = 300
	)

	grid := make([][]int, size)
	for y := 0; y < size; y++ {
		grid[y] = make([]int, size)
	}

	for y := 0; y < size; y++ {
		for x := 0; x < size; x++ {
			grid[y][x] = powerLevel(x+1, y+1, serialNumber)
		}
	}

	max := math.MinInt
	bestX := 0
	bestY := 0
	bestCell := 0
	for cell := 1; cell < size; cell++ {
		fmt.Println(cell)
		for y := 0; y < size-cell-1; y++ {
			for x := 0; x < size-cell-1; x++ {
				sum := 0
				for row := 0; row < cell; row++ {
					for col := 0; col < cell; col++ {
						sum += grid[y+row][x+col]
					}
				}

				if sum > max {
					max = sum
					bestX = x + 1
					bestY = y + 1
					bestCell = cell
				}
			}
		}
	}

	return fmt.Sprintf("%d,%d,%d", bestX, bestY, bestCell)
}

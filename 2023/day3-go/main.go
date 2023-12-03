package main

import (
	"io"

	aoc "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	rows := aoc.ReaderToStrings(input)
	res := 0

	for rowID, row := range rows {
		runes := []rune(row)
		for colID := 0; colID < len(runes); colID++ {
			if !aoc.IsRuneDecimal(runes[colID]) {
				continue
			}
			start := colID
			end := -1
			colID++
			for ; colID < len(runes); colID++ {
				if !aoc.IsRuneDecimal(runes[colID]) {
					end = colID - 1
					break
				}
			}
			if end == -1 {
				end = len(runes) - 1
			}
			colID = end

			number := 0
			for i := start; i <= end; i++ {
				number = number*10 + aoc.RuneToInt(runes[i])
			}

			// Left or right
			if isPeriod(rowID, start-1, rows) || isPeriod(rowID, end+1, rows) {
				res += number
				continue
			}
			// Top or down
			for col := start - 1; col <= end+1; col++ {
				if isPeriod(rowID-1, col, rows) || isPeriod(rowID+1, col, rows) {
					res += number
					break
				}
			}
		}
	}

	return res
}

func isPeriod(row, col int, rows []string) bool {
	if row < 0 || row >= len(rows) || col < 0 || col >= len(rows[0]) {
		return false
	}

	r := []rune(rows[row])[col]
	if aoc.IsRuneDecimal(r) {
		return false
	}
	return r != '.'
}

func fs2(input io.Reader) int {
	rows := aoc.ReaderToStrings(input)
	gearCount := make(map[aoc.Position]int)
	gearNumbers := make(map[aoc.Position][]int)

	for rowID, row := range rows {
		runes := []rune(row)
		for colID := 0; colID < len(runes); colID++ {
			if !aoc.IsRuneDecimal(runes[colID]) {
				continue
			}
			start := colID
			end := -1
			colID++
			for ; colID < len(runes); colID++ {
				if !aoc.IsRuneDecimal(runes[colID]) {
					end = colID - 1
					break
				}
			}
			if end == -1 {
				end = len(runes) - 1
			}
			colID = end

			number := 0
			for i := start; i <= end; i++ {
				number = number*10 + aoc.RuneToInt(runes[i])
			}

			// Left
			if isGear(rowID, start-1, rows) {
				pos := aoc.Position{Row: rowID, Col: start - 1}
				gearCount[pos]++
				gearNumbers[pos] = append(gearNumbers[pos], number)
				continue
			}
			// Right
			if isGear(rowID, end+1, rows) {
				pos := aoc.Position{Row: rowID, Col: end + 1}
				gearCount[pos]++
				gearNumbers[pos] = append(gearNumbers[pos], number)
				continue
			}
			for col := start - 1; col <= end+1; col++ {
				foundRow := -1
				// Top
				if isGear(rowID-1, col, rows) {
					foundRow = rowID - 1
				} else {
					// Down
					if isGear(rowID+1, col, rows) {
						foundRow = rowID + 1
					}
				}
				if foundRow == -1 {
					continue
				}

				pos := aoc.Position{Row: foundRow, Col: col}
				gearCount[pos]++
				gearNumbers[pos] = append(gearNumbers[pos], number)
				break
			}
		}
	}

	res := 0
	for pos, count := range gearCount {
		if count != 2 {
			continue
		}
		numbers := gearNumbers[pos]
		res += numbers[0] * numbers[1]
	}
	return res
}

func isGear(row, col int, rows []string) bool {
	if row < 0 || row >= len(rows) || col < 0 || col >= len(rows[0]) {
		return false
	}

	r := []rune(rows[row])[col]
	if aoc.IsRuneDecimal(r) {
		return false
	}
	return r == '*'
}

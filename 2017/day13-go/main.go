package main

import (
	"bufio"
	"fmt"
	lib "github.com/teivah/advent-of-code"
	"io"
)

func fs1(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	m := make(map[int]int)
	max := 0
	for scanner.Scan() {
		s := scanner.Text()
		del := lib.NewDelimiter(s, " ")
		k := lib.StringToInt(s[:del.Ind[0]-1])
		v := del.GetInt(1)
		m[k] = v
		max = lib.Max(max, k)
	}

	layers := make([]int, max+1)
	for i := 0; i <= max; i++ {
		if v, exists := m[i]; exists {
			layers[i] = v
		}
	}

	severity := 0
	scanners := make([]Scanner, max+1)
	for round := 0; round <= max; round++ {
		if layers[round] != 0 {
			if scanners[round].row == 0 {
				severity += round * layers[round]
			}
		}

		// Move scanners
		updateScanners(scanners, layers)
	}

	return severity
}

type Scanner struct {
	row int
	up  bool
}

func updateScanners(scanners []Scanner, layers []int) {
	for i := 0; i < len(scanners); i++ {
		if layers[i] == 0 || layers[i] == 1 {
			continue
		}

		scanner := scanners[i]
		if scanner.up {
			if scanner.row == 1 {
				scanners[i].up = false
			}
			scanners[i].row--
		} else {
			if scanner.row+2 == layers[i] {
				scanners[i].up = true
			}
			scanners[i].row++
		}
	}
}

func fs2(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	m := make(map[int]int)
	max := 0
	for scanner.Scan() {
		s := scanner.Text()
		del := lib.NewDelimiter(s, " ")
		k := lib.StringToInt(s[:del.Ind[0]-1])
		v := del.GetInt(1)
		m[k] = v
		max = lib.Max(max, k)
	}

	layers := make([]int, max+1)
	for i := 0; i <= max; i++ {
		if v, exists := m[i]; exists {
			layers[i] = v
		}
	}

	for delay := 0; ; delay++ {
		scanners := make([]Scanner, max+1)
		caught := false

		if delay%1000 == 0 {
			fmt.Println(delay)
		}

		for i := 0; i < delay; i++ {
			updateScanners(scanners, layers)
		}

		for round := 0; round <= max; round++ {
			if layers[round] != 0 {
				if scanners[round].row == 0 {
					caught = true
					break
				}
			}
			updateScanners(scanners, layers)
		}
		if !caught {
			return delay
		}
	}
}

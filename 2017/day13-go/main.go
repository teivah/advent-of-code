package main

import (
	"bufio"
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
	for round := 0; round <= max; round++ {
		if layers[round] != 0 {
			if getScannerPosition(layers[round], round) == 0 {
				severity += round * layers[round]
			}
		}
	}

	return severity
}

func getScannerPosition(layers int, round int) int {
	delta := round % (layers - 1)

	if round%((layers-1)*2) < layers-1 {
		return delta
	}

	return layers - 1 - delta
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
		caught := false

		for layer := 0; layer <= max; layer++ {
			picosecond := delay + layer
			if layers[layer] != 0 {
				if getScannerPosition(layers[layer], picosecond) == 0 {
					caught = true
					break
				}
			}
		}

		if !caught {
			return delay
		}
	}
}

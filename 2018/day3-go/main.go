package main

import (
	"bufio"
	"io"
	"strings"

	lib "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	var claims []Claim
	maxerRow := lib.NewMaxer()
	maxerCol := lib.NewMaxer()
	for scanner.Scan() {
		claim := toClaim(scanner.Text())
		claims = append(claims, claim)
		maxerRow.Add(claim.nbInchesTop + claim.height)
		maxerCol.Add(claim.nbInchesLeft + claim.width)
	}

	maxRow := maxerRow.Get()
	maxCol := maxerCol.Get()
	grid := make([][]int, maxRow)
	for i := 0; i < maxRow; i++ {
		grid[i] = make([]int, maxCol)
	}

	for _, claim := range claims {
		for row := 0; row < claim.height; row++ {
			for col := 0; col < claim.width; col++ {
				grid[row+claim.nbInchesTop][col+claim.nbInchesLeft]++
			}
		}
	}

	count := 0
	for i := 0; i < len(grid); i++ {
		for j := 0; j < len(grid[0]); j++ {
			v := grid[i][j]
			if v > 1 {
				count++
			}
		}
	}

	return count
}

type Claim struct {
	nbInchesLeft int
	nbInchesTop  int
	width        int
	height       int
}

func toClaim(s string) Claim {
	del := lib.NewDelimiter(s, " ")
	tmp := del.GetString(2)
	tmp = tmp[:len(tmp)-1]
	inches := strings.Split(tmp, ",")

	dims := strings.Split(del.GetString(3), "x")

	return Claim{
		nbInchesLeft: lib.StringToInt(inches[0]),
		nbInchesTop:  lib.StringToInt(inches[1]),
		width:        lib.StringToInt(dims[0]),
		height:       lib.StringToInt(dims[1]),
	}
}

func fs2(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	var claims []Claim
	maxerRow := lib.NewMaxer()
	maxerCol := lib.NewMaxer()
	for scanner.Scan() {
		claim := toClaim(scanner.Text())
		claims = append(claims, claim)
		maxerRow.Add(claim.nbInchesTop + claim.height)
		maxerCol.Add(claim.nbInchesLeft + claim.width)
	}

	maxRow := maxerRow.Get()
	maxCol := maxerCol.Get()
	grid := make([][]map[int]struct{}, maxRow)
	for i := 0; i < maxRow; i++ {
		grid[i] = make([]map[int]struct{}, maxCol)
	}

	for i, claim := range claims {
		for row := 0; row < claim.height; row++ {
			for col := 0; col < claim.width; col++ {
				if grid[row+claim.nbInchesTop][col+claim.nbInchesLeft] == nil {
					grid[row+claim.nbInchesTop][col+claim.nbInchesLeft] = make(map[int]struct{})
				}
				grid[row+claim.nbInchesTop][col+claim.nbInchesLeft][i] = struct{}{}
			}
		}
	}

	for i := 0; i < len(claims); i++ {
		overlap := false
	loop:
		for row := 0; row < len(grid); row++ {
			for col := 0; col < len(grid[0]); col++ {
				v := grid[row][col]
				_, exists := v[i]
				if len(v) > 1 && exists {
					overlap = true
					break loop
				}
			}
		}
		if !overlap {
			return i + 1
		}
	}
	return -1
}

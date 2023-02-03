package main

import (
	"bufio"
	"io"

	lib "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	maxer := lib.NewMaxer()
	for scanner.Scan() {
		line := scanner.Text()
		maxer.Add(seatID(position(line)))
	}

	return maxer.Get()
}

func seatID(pos lib.Position) int {
	return pos.Row*8 + pos.Col
}

func position(s string) lib.Position {
	l := 0
	r := 127
	for i := 0; i < 7; i++ {
		mid := l + (r-l)/2
		switch s[i] {
		case 'B':
			l = mid + 1
		case 'F':
			r = mid
		}
	}
	row := l

	l = 0
	r = 7
	for i := 7; i < 10; i++ {
		mid := l + (r-l)/2
		switch s[i] {
		case 'R':
			l = mid + 1
		case 'L':
			r = mid
		}
	}
	col := l

	return lib.Position{row, col}
}

func fs2(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	from := 10

	set := make(map[lib.Position]struct{})
	//for row := from; row <= 127; row++ {
	//	for col := 0; col <= 7; col++ {
	//		v, exists := set[row]
	//		if !exists {
	//			v = make(map[int]struct{})
	//			set[row] = v
	//		}
	//
	//		set[row][col] = struct{}{}
	//	}
	//}

	for scanner.Scan() {
		line := scanner.Text()
		pos := position(line)
		set[pos] = struct{}{}
	}

	for row := from; row <= 127; row++ {
		for col := 0; col <= 7; col++ {
			pos := lib.Position{row, col}
			_, contains := set[pos]
			if !contains {
				return seatID(pos)
			}
		}
	}

	return -1
}

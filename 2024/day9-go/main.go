package main

import (
	"fmt"
	"io"

	"github.com/teivah/go-aoc"
)

type cell struct {
	empty        bool
	digit        int
	alreadyMoved bool
}

func fs1(input io.Reader) int {
	in := aoc.ReaderToString(input)
	disk := make([]cell, 0, len(in))
	file := true
	fileID := 0
	for i := 0; i < len(in); i++ {
		n := aoc.RuneToInt(rune(in[i]))
		if file {
			for j := 0; j < n; j++ {
				disk = append(disk, cell{digit: fileID})
			}
			fileID++
		} else {
			for j := 0; j < n; j++ {
				disk = append(disk, cell{empty: true})
			}
		}
		file = !file
	}

	l := 0
	r := len(disk) - 1
	for l < r {
		if disk[r].empty {
			r--
			continue
		}
		if !disk[l].empty {
			l++
			continue
		}

		// At this stage, l and r are at the correct position.
		disk[l] = disk[r]
		disk[r] = cell{empty: true}
		l++
		r--
	}

	res := 0
	for position, c := range disk {
		if c.empty {
			continue
		}
		res += position * c.digit
	}
	return res
}

func printDisk(disk []cell) {
	for _, c := range disk {
		if c.empty {
			fmt.Printf(".")
		} else {
			fmt.Printf("x")
		}
	}
	fmt.Println()
}

type Empty struct {
	index int
	free  int
}

func fs2(input io.Reader) int {
	in := aoc.ReaderToString(input)
	disk := make([]cell, 0, len(in))
	file := true
	fileID := 0
	for i := 0; i < len(in); i++ {
		n := aoc.RuneToInt(rune(in[i]))
		if file {
			for j := 0; j < n; j++ {
				disk = append(disk, cell{digit: fileID})
			}
			fileID++
		} else {
			for j := 0; j < n; j++ {
				disk = append(disk, cell{empty: true})
			}
		}
		file = !file
	}

	var emptys []Empty
	for i := 0; i < len(disk); i++ {
		if !disk[i].empty {
			continue
		}
		j := i + 1
		for ; j < len(disk); j++ {
			if !disk[j].empty {
				break
			}
		}
		emptys = append(emptys, Empty{free: j - i, index: i})
		i = j
	}

	r := len(disk) - 1
	for r >= 0 {
		if disk[r].empty {
			r--
			continue
		}
		if disk[r].alreadyMoved {
			r--
			continue
		}
		j := r - 1
		for ; j >= 0; j-- {
			if disk[j].empty {
				break
			}
			if disk[j].digit != disk[r].digit {
				break
			}
		}
		length := r - j

		for pos, empty := range emptys {
			if empty.index > r {
				break
			}
			if length <= empty.free {
				for i := 0; i < length; i++ {
					disk[empty.index+i] = disk[r-i]
					disk[empty.index+i].alreadyMoved = true
					disk[r-i] = cell{empty: true}
				}
				if length == empty.free {
					emptys = append(emptys[:pos], emptys[pos+1:]...)
				} else {
					emptys[pos].free = empty.free - length
					emptys[pos].index += length
				}
				break
			}
		}
		r = r - length
	}

	res := 0
	for position, c := range disk {
		if c.empty {
			continue
		}
		res += position * c.digit
	}
	return res
}

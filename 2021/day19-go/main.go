package main

import (
	"bufio"
	"fmt"
	"io"

	aoc "github.com/teivah/advent-of-code"
)

/*
at least 12 overlapping beacon
24 directions
*/
func fs1(input io.Reader) int {
	groups := aoc.StringGroups(aoc.ReaderToStrings(input))

	m := make(map[int][]Position)
	for i, group := range groups {
		m[i] = toPositions(group)
	}

	findIntersections(m[0], m[1])

	return 42
}

func findIntersections(sc1, sc2 []Position) []Position {
	const (
		from  = -1500
		to    = 1500
		delta = 100
	)

	//contains := make(map[Position]bool)
	//for _, pos := range sc1 {
	//	contains[pos.reduce(delta)] = true
	//}
	//
	////var q []Position
	//for x := from / delta; x <= to/delta; x++ {
	//	for y := from / delta; y <= to/delta; y++ {
	//		for z := from / delta; z <= to/delta; z++ {
	//			sums := make(map[int]int)
	//			for _, pos := range sc2 {
	//				tmp := pos.reduce(delta)
	//				p2 := tmp.delta(x, y, z)
	//				for rotation, p := range p2.getAllRotations() {
	//					if contains[p] {
	//						sums[rotation]++
	//					}
	//				}
	//			}
	//			if x == 0 && y == -11 && z == 10 {
	//				fmt.Println(sums)
	//			}
	//
	//			for k, v := range sums {
	//				if v >= 12 {
	//					fmt.Println(k)
	//				}
	//			}
	//		}
	//	}
	//}

	contains := make(map[Position][]int)
	for _, pos := range sc1 {
		rotations := pos.getAllRotations()
		for id, rotation := range rotations {
			contains[rotation] = append(contains[rotation], id)
		}
	}
	x := 68
	y := -1246
	z := -43
	sums := make(map[int]int)
	for _, pos := range sc2 {
		d := Position{x, y, z}
		rotations := d.getAllRotations()
		for _, rotation := range rotations {
			p2 := pos.deltaPosition(rotation)
			if foundRotations, found := contains[p2]; found {
				for _, r := range foundRotations {
					sums[r]++
				}
			}
		}
	}

	fmt.Println(sums)

	return nil
}

type Position struct {
	x int
	y int
	z int
}

func (p Position) getAllRotations() []Position {
	var directions []Position
	x := p.x
	y := p.y
	z := p.z
	directions = append(directions, Position{x, y, z})
	directions = append(directions, Position{x, -y, -z})
	directions = append(directions, Position{x, -z, y})
	directions = append(directions, Position{-y, -z, x})
	directions = append(directions, Position{-x, -z, -y})
	directions = append(directions, Position{y, -z, -x})

	var rotations []Position
	for _, direction := range directions {
		x := direction.x
		y := direction.y
		z := direction.z
		rotations = append(rotations, Position{x, y, z})
		rotations = append(rotations, Position{-y, x, z})
		rotations = append(rotations, Position{-x, -y, z})
		rotations = append(rotations, Position{y, -x, z})
	}
	return rotations
}

func (p Position) delta(x, y, z int) Position {
	p.x += x
	p.y += y
	p.z += z
	return p
}

func (p Position) deltaPosition(p2 Position) Position {
	p.x += p2.x
	p.y += p2.y
	p.z += p2.z
	return p
}

func (p Position) reduce(n int) Position {
	p.x /= n
	p.y /= n
	p.z /= n
	return p
}

func toPositions(lines []string) []Position {
	var positions []Position
	for i := 1; i < len(lines); i++ {
		line := lines[i]
		del := aoc.NewDelimiter(line, ",")
		positions = append(positions, Position{
			x: del.GetInt(0),
			y: del.GetInt(1),
			z: del.GetInt(2),
		})
	}
	return positions
}

func fs2(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
		line := scanner.Text()
		_ = line
	}

	return 42
}

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

	var m [][]Position
	sum := 0
	for _, group := range groups {
		positions := toPositions(group)
		m = append(m, positions)
		sum += len(positions)
	}

	//one := findIntersections(m[0], m[1])
	//four := findIntersections(m[1], m[4])
	//for _, p := range four.getAllRotations() {
	//	fmt.Println(p.deltaPosition(one))
	//}

	//findIntersections(m[0], m[1])
	//findIntersections(m[1], m[4])
	//findIntersections(m[4], m[2])
	//findIntersections(m[1], m[3])

	//one, done := findIntersections(m[0], m[1])
	//four, dfour := findIntersections(m[1], m[4])
	//fmt.Println(one, four)
	//fmt.Println(done, dfour)

	overlaps := 0
	for i := 0; i < len(m); i++ {
		for j := i + 1; j < len(m); j++ {
			_, _, overlap := findIntersections(m[i], m[j])
			if overlap {
				overlaps++
			}
		}
	}

	fmt.Println(overlaps, sum)

	return sum - 12*overlaps
}

type Link struct {
	p1 Position
	p2 Position
}

func findIntersections(sc1, sc2 []Position) (Position, Position, bool) {
	exists := make(map[Position]bool)
	distances := make(map[Position][]Position)
	for i := 0; i < len(sc1); i++ {
		for j := i + 1; j < len(sc1); j++ {
			d := sc1[i].distance(sc1[j])
			exists[d] = true
			distances[sc1[i]] = append(distances[sc1[i]], d)
			distances[sc1[j]] = append(distances[sc1[j]], d)
		}
	}

	sums := make(map[int]int)
	distances2 := make(map[Position][]Position)
	for i := 0; i < len(sc2); i++ {
		for j := i + 1; j < len(sc2); j++ {
			for _, a := range sc2[i].getAllRotations() {
				for rotation, b := range sc2[j].getAllRotations() {
					d := a.distance(b)
					if exists[d] {
						sums[rotation]++
						distances2[sc2[i]] = append(distances[sc2[i]], d)
						distances2[sc2[j]] = append(distances[sc2[j]], d)
					}
				}
			}
		}
	}

	for _, v := range sums {
		if v >= 66 {
			// Overlap
			res := make(map[Position]map[Position]int)
			for from, d1 := range distances {
				for to, d2 := range distances2 {
					count := 0
					for _, d := range d2 {
						for _, x := range d1 {
							if d == x {
								count++
							}
						}
					}

					if count >= 1 {
						m, found := res[from]
						if !found {
							m = make(map[Position]int)
							res[from] = m
						}
						m[to]++
					}
				}
			}

			//unique := make(map[Position]Position)
			var unique []Link
			for k, v := range res {
				if len(v) == 1 {
					var p Position
					for k := range v {
						p = k
						break
					}
					unique = append(unique, Link{k, p})
					//unique[k] = p
				}
			}

			l0 := unique[0]
			l1 := unique[1]

			/*
				x y z
				-x y z
				-x -y z
				x -y z

				x -z y
				-x -z y
				-x z y
				x z y

				x -z y
				-x -z y
				-x z y
				x z y

				-y -z x
				y -z x
				y z x
				-y z x

				-x -z -y
				x -z -y
				x z -y
				-x z -y

				y -z -x
				-y -z -x
				-y z -x
				y z -x
			*/

			x, y, z := 0, 0, 0
			dx, dy, dz := 0, 0, 0

			if l0.p1.x+l0.p2.x == l1.p1.x+l1.p2.x {
				x = l0.p1.x + l0.p2.x
			} else if l0.p1.x-l0.p2.x == l1.p1.x-l1.p2.x {
				x = l0.p1.x - l0.p2.x
			} else if l0.p1.x+l0.p2.y == l1.p1.x+l1.p2.y {
				x = l0.p1.x + l0.p2.y
			} else if l0.p1.x-l0.p2.y == l1.p1.x-l1.p2.y {
				x = l0.p1.x - l0.p2.y
			} else if l0.p1.x+l0.p2.z == l1.p1.x+l1.p2.z {
				x = l0.p1.x + l0.p2.z
			} else if l0.p1.x-l0.p2.z == l1.p1.x-l1.p2.z {
				x = l0.p1.x - l0.p2.z
			}

			if l0.p1.y+l0.p2.y == l1.p1.y+l1.p2.y {
				y = l0.p1.y + l0.p2.y
			} else if l0.p1.y-l0.p2.y == l1.p1.y-l1.p2.y {
				y = l0.p1.y - l0.p2.y
			} else if l0.p1.y+l0.p2.x == l1.p1.y+l1.p2.x {
				y = l0.p1.y + l0.p2.x
			} else if l0.p1.y-l0.p2.x == l1.p1.y-l1.p2.x {
				y = l0.p1.y - l0.p2.x
			} else if l0.p1.y+l0.p2.z == l1.p1.y+l1.p2.z {
				y = l0.p1.y + l0.p2.z
			} else if l0.p1.y-l0.p2.z == l1.p1.y-l1.p2.z {
				y = l0.p1.y - l0.p2.z
			}

			if l0.p1.z+l0.p2.z == l1.p1.z+l1.p2.z {
				z = l0.p1.z + l0.p2.z
			} else if l0.p1.z-l0.p2.z == l1.p1.z-l1.p2.z {
				z = l0.p1.z - l0.p2.z
			} else if l0.p1.z+l0.p2.x == l1.p1.z+l1.p2.x {
				z = l0.p1.z + l0.p2.x
			} else if l0.p1.z-l0.p2.x == l1.p1.z-l1.p2.x {
				z = l0.p1.z - l0.p2.x
			} else if l0.p1.z+l0.p2.y == l1.p1.z+l1.p2.y {
				z = l0.p1.z + l0.p2.y
			} else if l0.p1.z-l0.p2.y == l1.p1.z-l1.p2.y {
				z = l0.p1.z - l0.p2.y
			}

			// ---

			if l0.p1.x+l0.p2.x == l1.p1.x+l1.p2.x {
				dx = x
			} else if l0.p1.x-l0.p2.x == l1.p1.x-l1.p2.x {
				dx = -x
			} else if l0.p1.x+l0.p2.y == l1.p1.x+l1.p2.y {
				dx = y
			} else if l0.p1.x-l0.p2.y == l1.p1.x-l1.p2.y {
				dx = -y
			} else if l0.p1.x+l0.p2.z == l1.p1.x+l1.p2.z {
				dx = z
			} else if l0.p1.x-l0.p2.z == l1.p1.x-l1.p2.z {
				dx = -z
			}

			if l0.p1.y+l0.p2.y == l1.p1.y+l1.p2.y {
				dy = y
			} else if l0.p1.y-l0.p2.y == l1.p1.y-l1.p2.y {
				dy = -y
			} else if l0.p1.y+l0.p2.x == l1.p1.y+l1.p2.x {
				dy = x
			} else if l0.p1.y-l0.p2.x == l1.p1.y-l1.p2.x {
				dy = -x
			} else if l0.p1.y+l0.p2.z == l1.p1.y+l1.p2.z {
				dy = z
			} else if l0.p1.y-l0.p2.z == l1.p1.y-l1.p2.z {
				dy = -z
			}

			if l0.p1.z+l0.p2.z == l1.p1.z+l1.p2.z {
				dz = z
			} else if l0.p1.z-l0.p2.z == l1.p1.z-l1.p2.z {
				dz = -z
			} else if l0.p1.z+l0.p2.x == l1.p1.z+l1.p2.x {
				dz = x
			} else if l0.p1.z-l0.p2.x == l1.p1.z-l1.p2.x {
				dz = -x
			} else if l0.p1.z+l0.p2.y == l1.p1.z+l1.p2.y {
				dz = y
			} else if l0.p1.z-l0.p2.y == l1.p1.z-l1.p2.y {
				dz = -y
			}

			pos := Position{x, y, z}
			pos2 := Position{dx, dy, dz}

			return pos, pos2, true
		}
	}

	return Position{}, Position{}, false
}

func testDelta(l0, l1 Link, dx, dy, dz int) bool {
	return l0.p1.x+dx*l0.p2.x == l1.p1.x+dx*l1.p2.x &&
		l0.p1.y+l0.p2.y == l1.p1.y+dy*l1.p2.y &&
		l0.p1.z+l0.p2.z == l1.p1.z+dz*l1.p2.z
}

type Position struct {
	x int
	y int
	z int
}

func (p Position) distance(p2 Position) Position {
	return Position{aoc.Abs(p.x - p2.x), aoc.Abs(p.y - p2.y), aoc.Abs(p.z - p2.z)}
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

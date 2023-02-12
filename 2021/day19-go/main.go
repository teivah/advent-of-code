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
	for _, group := range groups {
		m = append(m, toPositions(group))
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

	one, f := findIntersections(m[0], m[1])
	four, _ := findIntersections(m[1], m[4])

	fmt.Printf("one: %v, four: %v\n", one, four)
	fmt.Println(one.deltaPosition(f(four)))
	//fmt.Printf("done: %v, dfour: %v\n", done, dfour)

	//sum := 0
	//for i := 0; i < len(m); i++ {
	//	for j := i + 1; j < len(m); j++ {
	//		if v := findIntersections(m[i], m[j]); v != -1 {
	//			sum += v
	//		}
	//	}
	//}

	//return sum
	return 0
}

type Link struct {
	p1 Position
	p2 Position
}

func findIntersections(sc1, sc2 []Position) (Position, transform) {
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

			pos := Position{x, y, z}

			//fmt.Println(getRotation(unique, pos))
			f := getRotation2(unique, pos)
			fmt.Println(unique[0].p1, pos.deltaPosition(f(unique[0].p2)))

			// Gold
			//for _, v := range unique {
			//	for i, rotation := range v.p2.getAllRotations() {
			//		delta := pos.deltaPosition(rotation)
			//		if delta == v.p1 {
			//			fmt.Println(delta, i)
			//		}
			//	}
			//}

			return pos, f
		}
	}

	return Position{}, nil
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

type transform func(Position) Position

func getRotation2(unique []Link, foundPosition Position) transform {
	f := func(t transform, p2, initial Position) transform {
		p2 = t(p2)

		if foundPosition.deltaPosition(Position{p2.x, p2.y, p2.z}) == initial {
			return func(p Position) Position {
				return t(p)
			}
		}

		if foundPosition.deltaPosition(Position{-p2.y, p2.x, p2.z}) == initial {
			return func(p Position) Position {
				p = t(p)
				return Position{-p.y, p.x, p.z}
			}
		}

		if foundPosition.deltaPosition(Position{-p2.x, -p2.y, p2.z}) == initial {
			return func(p Position) Position {
				p = t(p)
				return Position{-p.x, -p.y, p.z}
			}
		}

		if foundPosition.deltaPosition(Position{p2.y, -p2.x, p2.z}) == initial {
			return func(p Position) Position {
				p = t(p)
				return Position{p.y, -p.x, p.z}
			}
		}
		return nil
	}

	for _, v := range unique {
		if h := f(func(p Position) Position {
			return p
		}, v.p2, v.p1); h != nil {
			return h
		}
		if h := f(func(p Position) Position {
			return Position{p.x, -p.y, -p.z}
		}, v.p2, v.p1); h != nil {
			return h
		}
		if h := f(func(p Position) Position {
			return Position{p.x, -p.z, p.y}
		}, v.p2, v.p1); h != nil {
			return h
		}
		if h := f(func(p Position) Position {
			return Position{-p.y, -p.z, p.x}
		}, v.p2, v.p1); h != nil {
			return h
		}
		if h := f(func(p Position) Position {
			return Position{-p.x, -p.z, -p.y}
		}, v.p2, v.p1); h != nil {
			return h
		}
		if h := f(func(p Position) Position {
			return Position{p.y, -p.z, -p.x}
		}, v.p2, v.p1); h != nil {
			return h
		}
	}
	panic(unique)
}

func getRotation(unique []Link, foundPosition Position) (int, int) {
	f := func(x, y, z int, initial Position) int {
		if foundPosition.deltaPosition((Position{x, y, z})) == initial {
			return 0
		}
		if foundPosition.deltaPosition((Position{-y, x, z})) == initial {
			return 1
		}
		if foundPosition.deltaPosition((Position{-x, -y, z})) == initial {
			return 2
		}
		if foundPosition.deltaPosition((Position{y, -x, z})) == initial {
			return 3
		}
		return -1
	}

	for _, v := range unique {
		if v := f(v.p2.x, v.p2.y, v.p2.z, v.p1); v != -1 {
			return 0, v
		}
		if v := f(v.p2.x, -v.p2.y, -v.p2.z, v.p1); v != -1 {
			return 1, v // -x +y -z
		}
		if v := f(v.p2.x, -v.p2.z, v.p2.y, v.p1); v != -1 {
			return 2, v
		}
		if v := f(-v.p2.y, -v.p2.z, v.p2.x, v.p1); v != -1 {
			return 3, v
		}
		if v := f(-v.p2.x, -v.p2.z, -v.p2.y, v.p1); v != -1 {
			return 4, v
		}
		if v := f(v.p2.y, -v.p2.z, -v.p2.x, v.p1); v != -1 {
			return 5, v
		}
	}
	panic(unique)
}

func (p Position) distance(p2 Position) Position {
	return Position{aoc.Abs(p.x - p2.x), aoc.Abs(p.y - p2.y), aoc.Abs(p.z - p2.z)}
}

func (p Position) getDirectionRotation(p2 Position) (int, int) {
	f := func(x, y, z int) int {
		if (Position{x, y, z}) == p2 {
			return 0
		}
		if (Position{-y, x, z}) == p2 {
			return 1
		}
		if (Position{-x, -y, z}) == p2 {
			return 2
		}
		if (Position{y, -x, z}) == p2 {
			return 3
		}
		return -1
	}

	if v := f(p.x, p.y, p.z); v != -1 {
		return 0, v
	}
	if v := f(p.x, -p.y, -p.z); v != -1 {
		return 1, v
	}
	if v := f(p.x, -p.z, p.y); v != -1 {
		return 2, v
	}
	if v := f(-p.y, -p.z, p.x); v != -1 {
		return 3, v
	}
	if v := f(-p.x, -p.z, -p.y); v != -1 {
		return 4, v
	}
	if v := f(p.y, -p.z, -p.x); v != -1 {
		return 5, v
	}
	return -1, -1
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

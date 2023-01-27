package main

import (
	"io"
	"sort"

	lib "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	lines := lib.ReaderToStrings(input)
	var points []Point
	for _, line := range lines {
		points = append(points, toPoint(line))
	}

	sort.Slice(points, func(i, j int) bool {
		a := points[i]
		b := points[j]
		return a.distance(Point{}) < b.distance(Point{})
	})

	var constellations [][]Point
	constellations = append(constellations, []Point{points[0]})

	for i := 1; i < len(points); i++ {
		point := points[i]
		added := false
		for id, constellation := range constellations {
			belong := false
			for _, cp := range constellation {
				if point.sameConstellation(cp) {
					belong = true
					break
				}
			}

			if belong {
				constellations[id] = append(constellations[id], point)
				added = true
				break
			}
		}

		if !added {
			constellations = append(constellations, []Point{point})
		}
	}

	res := len(constellations)

	for i := 0; i < len(constellations); i++ {
		for j := i + 1; j < len(constellations); j++ {
			if canGroup(constellations[i], constellations[j]) {
				res--
			}
		}
	}

	return res
}

func canGroup(c1, c2 []Point) bool {
	for _, p := range c1 {
		for _, p2 := range c2 {
			if p.sameConstellation(p2) {
				return true
			}
		}
	}
	return false
}

type Point struct {
	a int
	b int
	c int
	d int
}

func (p Point) sameConstellation(p2 Point) bool {
	return p.distance(p2) <= 3
}

func (p Point) distance(p2 Point) int {
	return lib.Abs(p.a-p2.a) +
		lib.Abs(p.b-p2.b) +
		lib.Abs(p.c-p2.c) +
		lib.Abs(p.d-p2.d)
}

func toPoint(s string) Point {
	del := lib.NewDelimiter(s, ",")
	return Point{
		a: del.GetInt(0),
		b: del.GetInt(1),
		c: del.GetInt(2),
		d: del.GetInt(3),
	}
}

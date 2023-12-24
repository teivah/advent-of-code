package main

import (
	"bufio"
	"io"

	aoc "github.com/teivah/advent-of-code"
	"gonum.org/v1/gonum/mat"
)

type Hailstone struct {
	position Coord
	velocity Coord
}

type Coord struct {
	x float64
	y float64
	z float64
}

func fs1(input io.Reader, from float64, to float64) int {
	scanner := bufio.NewScanner(input)
	var hailstones []Hailstone
	for scanner.Scan() {
		line := scanner.Text()
		hailstones = append(hailstones, parse(line))
	}

	count := 0
	for i := 0; i < len(hailstones); i++ {
		for j := i + 1; j < len(hailstones); j++ {

			x, y, intersect := findIntersection(hailstones[i], hailstones[j])
			if !intersect {
				continue
			}
			if x >= from && x <= to && y >= from && y <= to {
				count++
			}
		}
	}

	return count
}

func parse(s string) Hailstone {
	del := aoc.NewDelimiter(s, " @ ", aoc.WithTrimSpace())
	pos := aoc.NewDelimiter(del.GetString(0), ", ", aoc.WithTrimSpace()).GetInts()
	vel := aoc.NewDelimiter(del.GetString(1), ", ", aoc.WithTrimSpace()).GetInts()
	return Hailstone{
		position: Coord{
			x: float64(pos[0]),
			y: float64(pos[1]),
			z: float64(pos[2]),
		},
		velocity: Coord{
			x: float64(vel[0]),
			y: float64(vel[1]),
			z: float64(vel[2]),
		},
	}
}

func findIntersection(h2, h1 Hailstone) (float64, float64, bool) {
	pos1, vel1 := h2.position, h2.velocity
	pos2, vel2 := h1.position, h1.velocity

	// If parallel
	if vel1.x*vel2.y == vel1.y*vel2.x {
		return 0, 0, false
	}

	numerator := pos2.y - pos1.y + (pos1.x-pos2.x)*vel2.y/vel2.x
	denominator := vel1.y - vel1.x*vel2.y/vel2.x
	p1 := numerator / denominator

	if p1 < 0 {
		return 0, 0, false
	}

	p2 := (pos1.x - pos2.x + p1*vel1.x) / vel2.x

	if p2 < 0 {
		return 0, 0, false
	}

	x := pos1.x + p1*vel1.x
	y := pos1.y + p1*vel1.y

	return x, y, true
}

func fs2(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	var hailstones []Hailstone
	for scanner.Scan() {
		line := scanner.Text()
		hailstones = append(hailstones, parse(line))
	}

	return positionThrowObliterate(hailstones)
}

func positionThrowObliterate(hailstones []Hailstone) int {
	// Taking only the first 3 is working
	h0, h1, h2 := hailstones[0], hailstones[1], hailstones[2]

	data := []float64{
		0, h0.velocity.z - h1.velocity.z, h1.velocity.y - h0.velocity.y, 0, h1.position.z - h0.position.z, h0.position.y - h1.position.y,
		h1.velocity.z - h0.velocity.z, 0, h0.velocity.x - h1.velocity.x, h0.position.z - h1.position.z, 0, h1.position.x - h0.position.x,
		h0.velocity.y - h1.velocity.y, h1.velocity.x - h0.velocity.x, 0, h1.position.y - h0.position.y, h0.position.x - h1.position.x, 0,
		0, h0.velocity.z - h2.velocity.z, h2.velocity.y - h0.velocity.y, 0, h2.position.z - h0.position.z, h0.position.y - h2.position.y,
		h2.velocity.z - h0.velocity.z, 0, h0.velocity.x - h2.velocity.x, h0.position.z - h2.position.z, 0, h2.position.x - h0.position.x,
		h0.velocity.y - h2.velocity.y, h2.velocity.x - h0.velocity.x, 0, h2.position.y - h0.position.y, h0.position.x - h2.position.x, 0,
	}
	a := mat.NewDense(6, 6, data)

	vector := []float64{
		h0.position.y*h0.velocity.z - h0.velocity.y*h0.position.z - (h1.position.y*h1.velocity.z - h1.velocity.y*h1.position.z),
		h0.position.z*h0.velocity.x - h0.velocity.z*h0.position.x - (h1.position.z*h1.velocity.x - h1.velocity.z*h1.position.x),
		h0.position.x*h0.velocity.y - h0.velocity.x*h0.position.y - (h1.position.x*h1.velocity.y - h1.velocity.x*h1.position.y),
		h0.position.y*h0.velocity.z - h0.velocity.y*h0.position.z - (h2.position.y*h2.velocity.z - h2.velocity.y*h2.position.z),
		h0.position.z*h0.velocity.x - h0.velocity.z*h0.position.x - (h2.position.z*h2.velocity.x - h2.velocity.z*h2.position.x),
		h0.position.x*h0.velocity.y - h0.velocity.x*h0.position.y - (h2.position.x*h2.velocity.y - h2.velocity.x*h2.position.y),
	}
	b := mat.NewVecDense(6, vector)

	var x mat.VecDense
	if err := x.SolveVec(a, b); err != nil {
		panic(err)
	}

	sum := x.AtVec(0) + x.AtVec(1) + x.AtVec(2)
	return int(sum + 0.5) // rounding
}

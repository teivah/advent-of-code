package main

import (
	"bufio"
	"fmt"
	"io"

	lib "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader, steps int) int {
	scanner := bufio.NewScanner(input)
	var moons []*Moon
	for scanner.Scan() {
		moon := toMoon(scanner.Text())
		moons = append(moons, &moon)
	}

	for i := 0; i < steps; i++ {
		gravity(moons)
		velocity(moons)
	}

	totalEnergy := 0
	for _, moon := range moons {
		potentialEnery := lib.Abs(moon.x) + lib.Abs(moon.y) + lib.Abs(moon.z)
		kineticEnergy := lib.Abs(moon.vx) + lib.Abs(moon.vy) + lib.Abs(moon.vz)
		totalEnergy += potentialEnery * kineticEnergy
	}

	return totalEnergy
}

func gravity(moons []*Moon) {
	for i := 0; i < len(moons); i++ {
		for j := i + 1; j < len(moons); j++ {
			a := moons[i]
			b := moons[j]

			if a.x < b.x {
				moons[i].vx++
				moons[j].vx--
			} else if a.x > b.x {
				moons[j].vx++
				moons[i].vx--
			}

			if a.y < b.y {
				moons[i].vy++
				moons[j].vy--
			} else if a.y > b.y {
				moons[j].vy++
				moons[i].vy--
			}

			if a.z < b.z {
				moons[i].vz++
				moons[j].vz--
			} else if a.z > b.z {
				moons[j].vz++
				moons[i].vz--
			}
		}
	}
}

func velocity(moons []*Moon) {
	for i := range moons {
		moons[i].x += moons[i].vx
		moons[i].y += moons[i].vy
		moons[i].z += moons[i].vz
	}
}

type Moon struct {
	x  int
	y  int
	z  int
	vx int
	vy int
	vz int
}

func (m Moon) String() string {
	return fmt.Sprintf("pos=<x=%3d, y=%3d, z=%3d>, vel=<x=%3d, y=%3d, z=%3d>", m.x, m.y, m.z, m.vx, m.vy, m.vz)
}

func toMoon(s string) Moon {
	idx := lib.NewDelimiter(s, ",")
	return Moon{
		x: idx.GetInt(0),
		y: idx.GetInt(1),
		z: idx.GetInt(2),
	}
}

func fs2(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	var moons []*Moon
	for scanner.Scan() {
		moon := toMoon(scanner.Text())
		moons = append(moons, &moon)
	}

	type entry struct {
		moon1 Moon
		moon2 Moon
		moon3 Moon
		moon4 Moon
	}

	//for i := 0; ; i++ {
	//	fmt.Printf("%d: %v %d %d %d\n", i, moons[0].vz, moons[1].vz, moons[2].vz, moons[3].vz)
	//	gravity(moons)
	//	velocity(moons)
	//}

	// Running a series of print, we notice:
	// * x repeat each 186028 occurrences
	// * y repeat each 231614 occurrences
	// * z repeat each 102356 occurrences
	// So we simply return the least common multiple of these three numbers
	return leastCommonMultiple(186028, 231614, 102356)
}

func greatestCommonDivisor(a, b int) int {
	for b != 0 {
		t := b
		b = a % b
		a = t
	}
	return a
}

func leastCommonMultiple(a, b int, integers ...int) int {
	result := a * b / greatestCommonDivisor(a, b)
	for i := 0; i < len(integers); i++ {
		result = leastCommonMultiple(result, integers[i])
	}
	return result
}

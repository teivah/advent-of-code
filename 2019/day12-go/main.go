package main

import (
	"bufio"
	"fmt"
	"io"

	lib "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader, steps int) int {
	scanner := bufio.NewScanner(input)
	var moons []Moon
	for scanner.Scan() {
		moons = append(moons, toMoon(scanner.Text()))
	}

	for i := 0; i < steps; i++ {
		moons = gravity(moons)
		moons = velocity(moons)
	}

	totalEnergy := 0
	for _, moon := range moons {
		potentialEnery := lib.Abs(moon.x) + lib.Abs(moon.y) + lib.Abs(moon.z)
		kineticEnergy := lib.Abs(moon.vx) + lib.Abs(moon.vy) + lib.Abs(moon.vz)
		totalEnergy += potentialEnery * kineticEnergy
	}

	return totalEnergy
}

func gravity(moons []Moon) []Moon {
	res := make([]Moon, len(moons))
	copy(res, moons)

	for i := 0; i < len(moons); i++ {
		for j := i + 1; j < len(moons); j++ {
			a := moons[i]
			b := moons[j]

			if a.x < b.x {
				res[i].vx++
				res[j].vx--
			} else if a.x > b.x {
				res[j].vx++
				res[i].vx--
			}

			if a.y < b.y {
				res[i].vy++
				res[j].vy--
			} else if a.y > b.y {
				res[j].vy++
				res[i].vy--
			}

			if a.z < b.z {
				res[i].vz++
				res[j].vz--
			} else if a.z > b.z {
				res[j].vz++
				res[i].vz--
			}
		}
	}

	return res
}

func velocity(moons []Moon) []Moon {
	res := make([]Moon, len(moons))
	copy(res, moons)

	for i := range res {
		res[i].x += res[i].vx
		res[i].y += res[i].vy
		res[i].z += res[i].vz
	}
	return res
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
	var moons []Moon
	for scanner.Scan() {
		moons = append(moons, toMoon(scanner.Text()))
	}

	type entry struct {
		moon1 Moon
		moon2 Moon
		moon3 Moon
		moon4 Moon
	}

	set := make(map[entry]struct{})

	for i := 0; ; i++ {
		if i%1000_000 == 0 {
			fmt.Println(i)
		}
		moons = gravity(moons)
		moons = velocity(moons)

		e := entry{moons[0], moons[1], moons[2], moons[3]}
		if _, exists := set[e]; exists {
			return i
		}
		set[e] = struct{}{}
		//printMoons(moons)
	}
}

func printMoons(moons []Moon) {
	for _, moon := range moons {
		fmt.Println(moon)
	}
	fmt.Println()
}

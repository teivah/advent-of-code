package main

import (
	"bufio"
	"io"
	"math"

	lib "github.com/teivah/advent-of-code"
)

func fs1(moves int, input io.Reader) int {
	scanner := bufio.NewScanner(input)
	var particles []*Particle
	for scanner.Scan() {
		line := scanner.Text()
		spaceDel := lib.NewDelimiter(line, " ")
		particles = append(particles, &Particle{
			position:     toCoord(line[3 : spaceDel.Ind[0]-2]),
			velocity:     toCoord(line[spaceDel.Ind[0]+4 : spaceDel.Ind[1]-2]),
			acceleration: toCoord(line[spaceDel.Ind[1]+4 : len(line)-1]),
		})
	}

	closest := make(map[int]int)
	for move := 0; move < moves; move++ {
		minDistance := math.MaxInt
		res := -1
		for i, particle := range particles {
			particle.move()
			distance := particle.manhattan()
			if distance < minDistance {
				minDistance = distance
				res = i
			}
		}
		closest[res]++
	}

	best := -1
	max := -1
	for id, v := range closest {
		if v > max {
			max = v
			best = id
		}
	}

	return best
}

type Particle struct {
	position     Coord
	velocity     Coord
	acceleration Coord
}

func (p *Particle) move() {
	p.velocity.x += p.acceleration.x
	p.velocity.y += p.acceleration.y
	p.velocity.z += p.acceleration.z

	p.position.x += p.velocity.x
	p.position.y += p.velocity.y
	p.position.z += p.velocity.z
}

func (p *Particle) manhattan() int {
	return lib.Abs(p.position.x) + lib.Abs(p.position.y) + lib.Abs(p.position.z)
}

type Coord struct {
	x int
	y int
	z int
}

func toCoord(s string) Coord {
	del := lib.NewDelimiter(s, ",")
	return Coord{
		x: del.GetInt(0),
		y: del.GetInt(1),
		z: del.GetInt(2),
	}
}

func fs2(moves int, input io.Reader) int {
	scanner := bufio.NewScanner(input)
	particles := make(map[int]*Particle)
	i := -1
	for scanner.Scan() {
		i++
		line := scanner.Text()
		spaceDel := lib.NewDelimiter(line, " ")
		particles[i] = &Particle{
			position:     toCoord(line[3 : spaceDel.Ind[0]-2]),
			velocity:     toCoord(line[spaceDel.Ind[0]+4 : spaceDel.Ind[1]-2]),
			acceleration: toCoord(line[spaceDel.Ind[1]+4 : len(line)-1]),
		}
	}

	for move := 0; move < moves; move++ {
		positions := make(map[Coord][]int)
		for id, particle := range particles {
			particle.move()
			positions[particle.position] = append(positions[particle.position], id)
		}

		for _, ids := range positions {
			if len(ids) > 1 {
				for _, id := range ids {
					delete(particles, id)
				}
			}
		}
	}

	return len(particles)
}

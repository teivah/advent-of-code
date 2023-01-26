package main

import (
	"bufio"
	"fmt"
	"io"
	"math/rand"
	"sort"
	"strings"
	"time"

	lib "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	var bots []Bot

	maxRange := 0
	var maxBot Bot
	for scanner.Scan() {
		line := scanner.Text()
		bot := toBot(line)
		bots = append(bots, bot)
		if bot.r > maxRange {
			maxRange = bot.r
			maxBot = bot
		}
	}

	sum := 0
	for _, bot := range bots {
		if maxBot.hasInRange(bot.pos) {
			sum++
		}
	}

	return sum
}

type Bot struct {
	pos Position
	r   int
}

type Position struct {
	x int
	y int
	z int
}

func (p Position) Delta(x, y, z int) Position {
	p.x += x
	p.y += y
	p.z += z
	return p
}

func distance(a, b Position) int {
	return lib.Abs(a.x-b.x) + lib.Abs(a.y-b.y) + lib.Abs(a.z-b.z)
}

func (b Bot) hasInRange(pos Position) bool {
	return distance(b.pos, pos) <= b.r
}

func (b Bot) isInRange(b2 Bot) bool {
	return distance(b.pos, b2.pos) <= b2.r
}

func toBot(s string) Bot {
	coords := s[5:strings.Index(s, ">")]
	cdel := lib.NewDelimiter(coords, ",")

	space := strings.Index(s, " ")
	r := s[space+3:]

	return Bot{
		pos: Position{
			x: cdel.GetInt(0),
			y: cdel.GetInt(1),
			z: cdel.GetInt(2),
		},
		r: lib.StringToInt(r),
	}
}

func fs2(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	var bots []Bot

	for scanner.Scan() {
		line := scanner.Text()
		bot := toBot(line)
		bots = append(bots, bot)
	}

	type Found struct {
		occurence int
		id        int
	}
	var founds []Found
	for i := 0; i < len(bots); i++ {
		bot := bots[i]
		sum := 0
		for j := 0; j < len(bots); j++ {
			if bot.isInRange(bots[j]) {
				sum++
			}
		}
		founds = append(founds, Found{occurence: sum, id: i})
	}

	sort.Slice(founds, func(i, j int) bool {
		a := founds[i]
		b := founds[j]
		return a.occurence > b.occurence
	})

	bestValue := 0
	var bestPosition Position

	for j := 0; j < 100; j++ {
		botRange := bots[founds[0].id].r * 1000000
		cur := bots[founds[0].id].pos
		max := sumInRange(cur, bots)
		for i := 0; i < 10000; i++ {
			cur, max = find(max, cur, bots, botRange)
		}

		if max > bestValue {
			bestValue = max
			bestPosition = cur
			fmt.Println(bestValue, distance(Position{}, bestPosition))
		}
	}

	return bestValue
}

func find(max int, cur Position, bots []Bot, botRange int) (Position, int) {
	best := cur

	deltax := func(i int) Position {
		return cur.Delta(i, 0, 0)
	}
	deltay := func(i int) Position {
		return cur.Delta(0, i, 0)
	}
	deltaz := func(i int) Position {
		return cur.Delta(0, 0, i)
	}

	nx := search(deltax, -botRange, 0, bots)
	if nx != nil {
		v := sumInRange(*nx, bots)
		if v > max {
			max = v
			best = *nx
		}
	}
	px := search(deltax, 0, botRange, bots)
	if px != nil {
		v := sumInRange(*px, bots)
		if v > max {
			max = v
			best = *px
		}
	}

	ny := search(deltay, -botRange, 0, bots)
	if ny != nil {
		v := sumInRange(*ny, bots)
		if v > max {
			max = v
			best = *ny
		}
	}
	py := search(deltay, 0, botRange, bots)
	if py != nil {
		v := sumInRange(*py, bots)
		if v > max {
			max = v
			best = *py
		}
	}

	nz := search(deltaz, -botRange, 0, bots)
	if nz != nil {
		v := sumInRange(*nz, bots)
		if v > max {
			max = v
			best = *nz
		}
	}
	pz := search(deltaz, 0, botRange, bots)
	if pz != nil {
		v := sumInRange(*pz, bots)
		if v > max {
			max = v
			best = *pz
		}
	}

	return best, max
}

type Delta func(int) Position

func rnd() float64 {
	rand.Seed(time.Now().UnixNano())
	return 2 * rand.Float64()
}

func rndRange(r int) int {
	return r + int(float64(r)*rnd())
}

func search(delta Delta, from, to int, bots []Bot) *Position {
	from = rndRange(from)
	to = rndRange(to)

	vl := sumInRange(delta(from), bots)
	vr := sumInRange(delta(to), bots)
	max := lib.Max(vl, vr)

	l := from
	r := to
	for l < r {
		mid := l + (r-l)/2
		p := delta(mid)
		v := sumInRange(p, bots)

		if v > max {
			return &p
		}

		if vl < vr {
			l = mid + 1
		} else {
			r = mid - 1
		}
	}
	return nil
}

func sumInRange(pos Position, bots []Bot) int {
	sum := 0
	for _, b := range bots {
		if b.hasInRange(pos) {
			sum++
		}
	}
	return sum
}

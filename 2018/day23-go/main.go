package main

import (
	"bufio"
	"fmt"
	"io"
	"math"
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

	maxSumRange := 0
	minDistance := math.MaxInt
	found := false

	botRange := 100000000
	//var best Position
	var bests []Position
	for j := 0; j < 10000; j++ {
		//for j := 0; j < 1; j++ {
		cur := bots[founds[0].id].pos
		sumRange := sumInRange(cur, bots)
		for i := 0; i < 100; i++ {
			cur, sumRange, found = find(sumRange, cur, bots, botRange)
			if !found {
				break
			}
		}

		curDistance := distance(Position{}, cur)

		if sumRange > maxSumRange {
			maxSumRange = sumRange
			minDistance = curDistance
			bests = []Position{cur}
			//best = cur
			fmt.Println(maxSumRange, minDistance)
		} else if sumRange == maxSumRange {
			bests = append(bests, cur)
			if curDistance < minDistance {
				minDistance = curDistance
				//best = cur
				fmt.Println(maxSumRange, minDistance)
			}
		}
	}

	//fmt.Println(best, minDistance)
	//best = Position{12693513, 30274062, 43552241}
	//maxSumRange = 919
	//minDistance = 86519816
	fmt.Println("step 2")

	//for _, best := range bests {
	//	const buffer = 10
	//	for x := -buffer; x < buffer; x++ {
	//		for y := -buffer; y < buffer; y++ {
	//			for z := -buffer; z < buffer; z++ {
	//				pos := best.Delta(x, y, z)
	//				sumRange := sumInRange(pos, bots)
	//				if sumRange != maxSumRange {
	//					continue
	//				}
	//				d := distance(pos, Position{})
	//				if d < minDistance {
	//					minDistance = d
	//					fmt.Println(minDistance)
	//				}
	//			}
	//		}
	//	}
	//}

	for _, best := range bests {
		px := 1
		lastDistance := minDistance
		for ; ; px++ {
			pos := best.Delta(px, 0, 0)
			sumRange := sumInRange(pos, bots)
			if sumRange != maxSumRange {
				break
			}
			d := distance(pos, Position{})
			if d > lastDistance {
				break
			}
			lastDistance = d
		}

		nx := -1
		lastDistance = minDistance
		for ; ; nx++ {
			pos := best.Delta(nx, 0, 0)
			sumRange := sumInRange(pos, bots)
			if sumRange != maxSumRange {
				break
			}
			d := distance(pos, Position{})
			if d > lastDistance {
				break
			}
			lastDistance = d
		}

		py := 1
		lastDistance = minDistance
		for ; ; py++ {
			pos := best.Delta(0, py, 0)
			sumRange := sumInRange(pos, bots)
			if sumRange != maxSumRange {
				break
			}
			d := distance(pos, Position{})
			if d > lastDistance {
				break
			}
			lastDistance = d
		}

		ny := -1
		lastDistance = minDistance
		for ; ; ny-- {
			pos := best.Delta(0, ny, 0)
			sumRange := sumInRange(pos, bots)
			if sumRange != maxSumRange {
				break
			}
			d := distance(pos, Position{})
			if d > lastDistance {
				break
			}
			lastDistance = d
		}

		pz := 1
		lastDistance = minDistance
		for ; ; pz++ {
			pos := best.Delta(0, pz, 0)
			sumRange := sumInRange(pos, bots)
			if sumRange != maxSumRange {
				break
			}
			d := distance(pos, Position{})
			if d > lastDistance {
				break
			}
			lastDistance = d
		}

		nz := -1
		lastDistance = minDistance
		for ; ; nz-- {
			pos := best.Delta(0, nz, 0)
			sumRange := sumInRange(pos, bots)
			if sumRange != maxSumRange {
				break
			}
			d := distance(pos, Position{})
			if d > lastDistance {
				break
			}
			lastDistance = d
		}

		fmt.Println(px, nx, py, ny, pz, nz)
	}

	//var best2 Position
	//for j := 0; j < 1000000; j++ {
	//	cur := best
	//	for i := 0; i < 100; i++ {
	//		cur, _, found = findDistance(maxSumRange, minDistance, cur, bots, botRange)
	//		if !found {
	//			break
	//		}
	//	}
	//
	//	curDistance := distance(Position{}, cur)
	//
	//	if curDistance < minDistance {
	//		minDistance = curDistance
	//		best2 = cur
	//		fmt.Println(maxSumRange, minDistance)
	//	}
	//}
	//
	//fmt.Println(best2)

	return minDistance
}

func find(max int, cur Position, bots []Bot, botRange int) (Position, int, bool) {
	initMax := max
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

	nx, v := search(deltax, -botRange, 0, bots)
	if nx != nil {
		if v > max {
			max = v
			best = *nx
		}
	}
	px, v := search(deltax, 0, botRange, bots)
	if px != nil {
		if v > max {
			max = v
			best = *px
		}
	}

	ny, v := search(deltay, -botRange, 0, bots)
	if ny != nil {
		if v > max {
			max = v
			best = *ny
		}
	}
	py, v := search(deltay, 0, botRange, bots)
	if py != nil {
		if v > max {
			max = v
			best = *py
		}
	}

	nz, v := search(deltaz, -botRange, 0, bots)
	if nz != nil {
		if v > max {
			max = v
			best = *nz
		}
	}
	pz, v := search(deltaz, 0, botRange, bots)
	if pz != nil {
		if v > max {
			max = v
			best = *pz
		}
	}

	return best, max, max != initMax
}

func findDistance(d int, min int, cur Position, bots []Bot, botRange int) (Position, int, bool) {
	initMin := min
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

	nx, v := searchDistance(d, deltax, -botRange, 0, bots)
	if nx != nil {
		if v < min {
			min = v
			best = *nx
		}
	}
	px, v := searchDistance(d, deltax, 0, botRange, bots)
	if px != nil {
		if v < min {
			min = v
			best = *px
		}
	}

	ny, v := searchDistance(d, deltay, -botRange, 0, bots)
	if ny != nil {
		if v < min {
			min = v
			best = *ny
		}
	}
	py, v := searchDistance(d, deltay, 0, botRange, bots)
	if py != nil {
		if v < min {
			min = v
			best = *py
		}
	}

	nz, v := searchDistance(d, deltaz, -botRange, 0, bots)
	if nz != nil {
		if v < min {
			min = v
			best = *nz
		}
	}
	pz, v := searchDistance(d, deltaz, 0, botRange, bots)
	if pz != nil {
		if v < min {
			min = v
			best = *pz
		}
	}

	return best, min, min != initMin
}

type Delta func(int) Position

func rnd() float64 {
	rand.Seed(time.Now().UnixNano())
	return 2 * rand.Float64()
}

func rndRange(r int) int {
	return r + int(float64(r)*rnd())
}

func search(delta Delta, from, to int, bots []Bot) (*Position, int) {
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
			return &p, v
		}

		if vl < vr {
			l = mid + 1
		} else {
			r = mid - 1
		}
	}
	return nil, 0
}

func searchDistance(d int, delta Delta, from, to int, bots []Bot) (*Position, int) {
	from = rndRange(from)
	to = rndRange(to)

	vl := distance(delta(from), Position{})
	vr := distance(delta(to), Position{})
	min := lib.Min(vl, vr)

	l := from
	r := to
	for l < r {
		mid := l + (r-l)/2
		p := delta(mid)
		if sumInRange(p, bots) != d {
			if vl < vr {
				r = mid - 1
			} else {
				l = mid + 1
			}
			continue
		}

		v := distance(p, Position{})

		if v < min {
			return &p, v
		}

		if vl < vr {
			r = mid - 1
		} else {
			l = mid + 1
		}
	}
	return nil, 0
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

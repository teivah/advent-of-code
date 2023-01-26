package main

import (
	"bufio"
	"fmt"
	"io"
	"math"
	"strings"

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

func distanceDiv(a, b Position, div int) int {
	return lib.Abs(a.x/div-b.x) + lib.Abs(a.y/div-b.y) + lib.Abs(a.z/div-b.z)
}

func (b Bot) hasInRange(pos Position) bool {
	return distance(b.pos, pos) <= b.r
}

func (b Bot) hasInRangeDiv(pos Position, div int) bool {
	d := distanceDiv(b.pos, pos, div)
	return d <= b.r/div
}

func (b Bot) isInRange(b2 Bot) bool {
	return distance(b.pos, b2.pos) <= b2.r
}

func (b Bot) isInRangeDiv(b2 Bot, div int) bool {
	return distanceDiv(b.pos, b2.pos, div) <= b2.r/div
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

	minX := math.MaxInt
	minY := math.MaxInt
	minZ := math.MaxInt
	maxX := math.MinInt
	maxY := math.MinInt
	maxZ := math.MinInt
	for scanner.Scan() {
		line := scanner.Text()
		bot := toBot(line)
		bots = append(bots, bot)
		minX = lib.Min(minX, bot.pos.x-bot.r)
		minY = lib.Min(minY, bot.pos.y-bot.r)
		minZ = lib.Min(minZ, bot.pos.z-bot.r)
		maxX = lib.Max(maxX, bot.pos.x+bot.r)
		maxY = lib.Max(maxY, bot.pos.y+bot.r)
		maxZ = lib.Max(maxZ, bot.pos.z+bot.r)
	}

	var best Position
	max := 0
	div := 1_000_000
	for x := minX / div; x < maxX/div; x++ {
		fmt.Println(x)
		for y := minY / div; y < maxY/div; y++ {
			for z := minZ / div; z < maxZ/div; z++ {
				pos := Position{x, y, z}
				inRange := sumInRangeDiv(pos, bots, div)
				if inRange > max {
					fmt.Println(inRange)
					max = inRange
					best = pos
				}
			}
		}
	}

	div /= 10
	best = Position{12, 30, 42}
	for ; div > 0; div /= 10 {
		max = 0
		minX = (best.x - 1) * 10
		minY = (best.y - 1) * 10
		minZ = (best.z - 1) * 10
		maxX = (best.x + 1) * 10
		maxY = (best.y + 1) * 10
		maxZ = (best.z + 1) * 10

		for x := minX; x < maxX; x++ {
			fmt.Println(x)
			for y := minY; y < maxY; y++ {
				for z := minZ; z < maxZ; z++ {
					pos := Position{x, y, z}
					inRange := sumInRangeDiv(pos, bots, div)
					if inRange > max {
						max = inRange
						best = pos
					}
				}
			}
		}
	}

	return distance(Position{}, best)
}

func sumInRangeDiv(pos Position, bots []Bot, div int) int {
	sum := 0
	for _, b := range bots {
		if b.hasInRangeDiv(pos, div) {
			sum++
		}
	}
	return sum
}

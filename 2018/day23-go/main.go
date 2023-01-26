package main

import (
	"bufio"
	"io"
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
		if maxBot.isInRange(bot.pos) {
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

func distance(a, b Position) int {
	return lib.Abs(a.x-b.x) + lib.Abs(a.y-b.y) + lib.Abs(a.z-b.z)
}

func (b Bot) isInRange(pos Position) bool {
	return distance(b.pos, pos) <= b.r
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
	for scanner.Scan() {
		line := scanner.Text()
		_ = line
	}

	return 42
}

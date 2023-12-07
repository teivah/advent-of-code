package main

import (
	"bufio"
	"io"
	"sort"

	aoc "github.com/teivah/advent-of-code"
)

type CardValue int
type HandType int

const (
	two CardValue = iota + 2
	three
	four
	five
	six
	seven
	eight
	nine
	ten
	jack
	queen
	king
	as
)

const (
	highCard HandType = iota
	onePair
	twoPair
	threeOfAKind
	fullHouse
	fourOfAKind
	fiveOfAKind
)

func fs1(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	var players []Player
	for scanner.Scan() {
		line := scanner.Text()
		p := toPlayer(line)
		players = append(players, p)
	}

	sort.Slice(players, func(i, j int) bool {
		p1 := players[i]
		p2 := players[j]
		if p1.handType < p2.handType {
			return true
		}
		if p1.handType > p2.handType {
			return false
		}
		for idx, p1Card := range p1.hand {
			p2Card := p2.hand[idx]
			if p1Card < p2Card {
				return true
			}
			if p2Card < p1Card {
				return false
			}
		}
		panic("same hand")
	})

	res := 0
	for i, p := range players {
		res += p.bet * (i + 1)
	}

	return res
}

type Player struct {
	hand     []CardValue
	handType HandType
	bet      int
}

func toPlayer(s string) Player {
	del := aoc.NewDelimiter(s, " ", aoc.WithTrimSpace())
	elems := del.GetStrings()

	var hand []CardValue
	for _, c := range elems[0] {
		switch c {
		case '2':
			hand = append(hand, two)
		case '3':
			hand = append(hand, three)
		case '4':
			hand = append(hand, four)
		case '5':
			hand = append(hand, five)
		case '6':
			hand = append(hand, six)
		case '7':
			hand = append(hand, seven)
		case '8':
			hand = append(hand, eight)
		case '9':
			hand = append(hand, nine)
		case 'T':
			hand = append(hand, ten)
		case 'J':
			hand = append(hand, jack)
		case 'Q':
			hand = append(hand, queen)
		case 'K':
			hand = append(hand, king)
		case 'A':
			hand = append(hand, as)
		default:
			panic(c)
		}
	}

	return Player{
		hand:     hand,
		handType: toHandType(hand),
		bet:      aoc.StringToInt(elems[1]),
	}
}

func toHandType(hand []CardValue) HandType {
	m := make(map[CardValue]int)
	for _, card := range hand {
		m[card]++
	}

	if isCount(m, 5) {
		return fiveOfAKind
	}
	if isCount(m, 4) {
		return fourOfAKind
	}
	if isCount(m, 3) && isCount(m, 2) {
		return fullHouse
	}
	if isCount(m, 3) {
		return threeOfAKind
	}
	if isTwoPair(m) {
		return twoPair
	}
	if isCount(m, 2) {
		return onePair
	}
	return highCard
}

func isCount(m map[CardValue]int, same int) bool {
	for _, frequency := range m {
		if frequency == same {
			return true
		}
	}
	return false
}

func isTwoPair(m map[CardValue]int) bool {
	pair := 0
	for _, frequency := range m {
		if frequency == 2 {
			pair++
		}
	}
	return pair == 2
}

func fs2(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	var players []Player
	for scanner.Scan() {
		line := scanner.Text()
		p := toPlayer2(line)
		players = append(players, p)
	}

	sort.Slice(players, func(i, j int) bool {
		p1 := players[i]
		p2 := players[j]

		if p1.handType < p2.handType {
			return true
		}
		if p1.handType > p2.handType {
			return false
		}
		for idx, p1Card := range p1.hand {
			p2Card := p2.hand[idx]
			if p1Card == p2Card {
				continue
			}

			// Jack special cases
			if p1Card == jack {
				return true
			}
			if p2Card == jack {
				return false
			}

			if p1Card < p2Card {
				return true
			}
			if p2Card < p1Card {
				return false
			}
		}
		panic("same hand")
	})

	res := 0
	for i, p := range players {
		res += p.bet * (i + 1)
	}

	return res
}

func toPlayer2(s string) Player {
	del := aoc.NewDelimiter(s, " ", aoc.WithTrimSpace())
	elems := del.GetStrings()

	var hand []CardValue
	jFrequency := 0
	for _, c := range elems[0] {
		switch c {
		case '2':
			hand = append(hand, two)
		case '3':
			hand = append(hand, three)
		case '4':
			hand = append(hand, four)
		case '5':
			hand = append(hand, five)
		case '6':
			hand = append(hand, six)
		case '7':
			hand = append(hand, seven)
		case '8':
			hand = append(hand, eight)
		case '9':
			hand = append(hand, nine)
		case 'T':
			hand = append(hand, ten)
		case 'J':
			hand = append(hand, jack)
			jFrequency++
		case 'Q':
			hand = append(hand, queen)
		case 'K':
			hand = append(hand, king)
		case 'A':
			hand = append(hand, as)
		default:
			panic(c)
		}
	}

	var handType HandType
	if jFrequency == 0 {
		handType = toHandType(hand)
	} else {
		handType = toHandType2(hand)
	}

	return Player{
		hand:     hand,
		handType: handType,
		bet:      aoc.StringToInt(elems[1]),
	}
}

func toHandType2(hand []CardValue) HandType {
	jFrequency := 0
	for _, card := range hand {
		if card == jack {
			jFrequency++
		}
	}

	m := make(map[CardValue]int)
	for _, card := range hand {
		m[card]++
	}

	highestCard := getBestCard(m)
	m[highestCard] += jFrequency
	m[jack] = 0

	if isCount2(m, 5) {
		return fiveOfAKind
	}
	if isCount2(m, 4) {
		return fourOfAKind
	}
	if isCount2(m, 3) && isCount(m, 2) {
		return fullHouse
	}
	if isCount2(m, 3) {
		return threeOfAKind
	}
	if isTwoPair2(m) {
		return twoPair
	}
	if isCount2(m, 2) {
		return onePair
	}
	return highCard
}

func getBestCard(m map[CardValue]int) CardValue {
	var highestCard CardValue
	highestFrequency := 0
	for card, frequency := range m {
		if card == jack {
			continue
		}

		if frequency > highestFrequency {
			highestCard = card
			highestFrequency = frequency
			continue
		}
		if frequency < highestFrequency {
			continue
		}
		highestCard = max(highestCard, card)
	}
	return highestCard
}

func isCount2(m map[CardValue]int, same int) bool {
	for card, frequency := range m {
		if card == jack {
			continue
		}
		if frequency == same {
			return true
		}
	}
	return false
}

func isTwoPair2(m map[CardValue]int) bool {
	pair := 0
	for card, frequency := range m {
		if card == jack {
			continue
		}
		if frequency == 2 {
			pair++
		}
	}
	return pair == 2
}

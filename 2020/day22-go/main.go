package main

import (
	"fmt"
	"io"

	aoc "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	players := stringGroups(aoc.ReaderToStrings(input))
	player1 := toPlayer(players[0])
	player2 := toPlayer(players[1])

	for len(player1.cards) != 0 && len(player2.cards) != 0 {
		c1 := player1.cards[0]
		player1.cards = player1.cards[1:]
		c2 := player2.cards[0]
		player2.cards = player2.cards[1:]

		if c1 > c2 {
			player1.cards = append(player1.cards, c1)
			player1.cards = append(player1.cards, c2)
		} else {
			player2.cards = append(player2.cards, c2)
			player2.cards = append(player2.cards, c1)
		}
	}

	cards := player1.cards
	if len(player1.cards) == 0 {
		cards = player2.cards
	}

	score := 0
	for i := 0; i < len(cards); i++ {
		score += (len(cards) - i) * cards[i]
	}

	return score
}

func toPlayer(lines []string) Player {
	var cards []int
	for i := 1; i < len(lines); i++ {
		cards = append(cards, aoc.StringToInt(lines[i]))
	}
	return Player{cards: cards}
}

type Player struct {
	cards []int
}

func stringGroups(lines []string) [][]string {
	i := 0
	var res [][]string
	var row []string
	for {
		row = append(row, lines[i])
		i++
		if i >= len(lines) {
			res = append(res, row)
			break
		}
		for ; i < len(lines); i++ {
			if lines[i] == "" {
				break
			} else {
				row = append(row, lines[i])
			}
		}
		res = append(res, row)
		row = nil
		i++
		if i >= len(lines) {
			break
		}
	}
	return res
}

func fs2(input io.Reader) int {
	players := stringGroups(aoc.ReaderToStrings(input))
	player1 := toPlayer(players[0])
	player2 := toPlayer(players[1])

	rc := newGame(player1.cards, player2.cards)
	winner := rc.game()

	cards := rc.cards1
	if winner == winnerPlayer2 {
		cards = rc.cards2
	}

	score := 0
	for i := 0; i < len(cards); i++ {
		score += (len(cards) - i) * cards[i]
	}

	return score
}

type Winner int

const (
	winnerPlayer1 Winner = iota
	winnerPlayer2
)

type RecursiveCombat struct {
	previousRound map[string]bool
	cards1        []int
	cards2        []int
	over          bool
}

func newGame(cards1, cards2 []int) *RecursiveCombat {
	return &RecursiveCombat{
		previousRound: make(map[string]bool),
		cards1:        copyCards(cards1),
		cards2:        copyCards(cards2),
	}
}

func (rc *RecursiveCombat) game() Winner {
	for len(rc.cards1) != 0 && len(rc.cards2) != 0 {
		rc.round()
		if rc.over {
			return winnerPlayer1
		}
	}

	if len(rc.cards1) == 0 {
		return winnerPlayer2
	}
	return winnerPlayer1
}

func (rc *RecursiveCombat) round() {
	k := rc.key()
	if rc.previousRound[k] {
		rc.over = true
	}
	rc.previousRound[k] = true

	c1 := rc.cards1[0]
	rc.cards1 = rc.cards1[1:]
	c2 := rc.cards2[0]
	rc.cards2 = rc.cards2[1:]

	var winner Winner
	if len(rc.cards1) >= c1 && len(rc.cards2) >= c2 {
		winner = newGame(rc.cards1[:c1], rc.cards2[:c2]).game()
	} else {
		if c1 > c2 {
			winner = winnerPlayer1
		} else {
			winner = winnerPlayer2
		}
	}

	if winner == winnerPlayer1 {
		rc.cards1 = append(rc.cards1, c1)
		rc.cards1 = append(rc.cards1, c2)
	} else {
		rc.cards2 = append(rc.cards2, c2)
		rc.cards2 = append(rc.cards2, c1)
	}
}

func (rc *RecursiveCombat) key() string {
	return fmt.Sprintf("%v-%v", rc.cards1, rc.cards2)
}

func copyCards(cards []int) []int {
	res := make([]int, len(cards))
	copy(res, cards)
	return res
}

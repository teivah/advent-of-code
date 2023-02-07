package main

import (
	"io"

	aoc "github.com/teivah/advent-of-code"
)

const bingoLength = 5

func fs1(input io.Reader) int {
	groups := aoc.StringGroups(aoc.ReaderToStrings(input))
	bingo := toBingo(groups)

	for i := 0; i < len(bingo.cards); i++ {
		card := bingo.cards[i]
		for j := 0; j < len(bingo.players); j++ {
			player := bingo.players[j]
			player.mark(card)
			if player.isWinner() {
				return player.score(card)
			}
		}
	}

	return 42
}

func toBingo(groups [][]string) *Bingo {
	del := aoc.NewDelimiter(groups[0][0], ",")
	cards := del.GetInts()

	var players []*Player
	for i := 1; i < len(groups); i++ {
		pCards := make([][]int, bingoLength)
		for row := 0; row < bingoLength; row++ {
			pCards[row] = make([]int, 0, bingoLength)
			s := groups[i][row]
			for j := 0; j < len(s); j++ {
				r := rune(s[j])
				if aoc.IsRuneDecimal(r) {
					k := j + 1
					for ; k < len(s); k++ {
						if !aoc.IsRuneDecimal(rune(s[k])) {
							break
						}
					}
					v := s[j:k]
					pCards[row] = append(pCards[row], aoc.StringToInt(v))
					j = k
				}
			}
		}
		players = append(players, &Player{cards: pCards, marked: newMarked()})
	}

	return &Bingo{
		cards:   cards,
		players: players,
	}
}

type Bingo struct {
	cards   []int
	players []*Player
}

type Player struct {
	cards  [][]int
	marked [][]bool
}

func newMarked() [][]bool {
	res := make([][]bool, bingoLength)
	for i := 0; i < bingoLength; i++ {
		res[i] = make([]bool, bingoLength)
	}
	return res
}

func (p *Player) mark(v int) {
	for row := 0; row < bingoLength; row++ {
		for col := 0; col < bingoLength; col++ {
			if p.cards[row][col] == v {
				p.marked[row][col] = true
			}
		}
	}
}

func (p *Player) isWinner() bool {
	for row := 0; row < bingoLength; row++ {
		win := true
		for col := 0; col < bingoLength; col++ {
			if !p.marked[row][col] {
				win = false
				break
			}
		}
		if win {
			return true
		}
	}
	for col := 0; col < bingoLength; col++ {
		win := true
		for row := 0; row < bingoLength; row++ {
			if !p.marked[row][col] {
				win = false
				break
			}
		}
		if win {
			return true
		}
	}
	return false
}

func (p *Player) score(v int) int {
	sum := 0
	for row := 0; row < bingoLength; row++ {
		for col := 0; col < bingoLength; col++ {
			if !p.marked[row][col] {
				sum += p.cards[row][col]
			}
		}
	}
	return sum * v
}

func fs2(input io.Reader) int {
	groups := aoc.StringGroups(aoc.ReaderToStrings(input))
	bingo := toBingo(groups)

	remaining := make(map[int]bool)
	for i := range bingo.players {
		remaining[i] = true
	}

	for i := 0; i < len(bingo.cards); i++ {
		card := bingo.cards[i]
		for j := 0; j < len(bingo.players); j++ {
			player := bingo.players[j]
			player.mark(card)
			if player.isWinner() {
				delete(remaining, j)
				if len(remaining) == 0 {
					return player.score(card)
				}
			}
		}
	}

	return 42
}

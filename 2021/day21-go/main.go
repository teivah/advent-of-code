package main

func fs1(p1 int, p2 int) int {
	score1 := 0
	score2 := 0

	dice := newDice()
	for {
		p1 = play(p1, dice)
		score1 += p1 + 1
		if score1 >= 1000 {
			return score2 * dice.rolled
		}

		p2 = play(p2, dice)
		score2 += p2 + 1
		if score1 >= 1000 {
			return score1 * dice.rolled
		}
	}
}

func play(p int, d *Dice) int {
	v := d.roll() + d.roll() + d.roll()

	return (p + v) % 10
}

type Dice struct {
	i      int
	rolled int
}

func newDice() *Dice {
	return &Dice{
		i: 1,
	}
}

func (d *Dice) roll() int {
	d.rolled++
	v := d.i
	d.i++
	if d.i > 100 {
		d.i = 1
	}
	return v
}

func fs2(p1 int, p2 int) int {
	a, b := dirac(true, 0, 0, p1, p2, make(map[Key]Value), 1)
	if a > b {
		return a
	}
	return b
}

type Key struct {
	player1ToPlay bool
	score1        int
	score2        int
	p1            int
	p2            int
	universe      int
}

type Value struct {
	p1 int
	p2 int
}

func init() {
	calcOptions(0, 3)
}

func calcOptions(sum, dice int) {
	if dice == 0 {
		options[sum]++
		return
	}
	for i := 1; i <= 3; i++ {
		calcOptions(sum+i, dice-1)
	}
}

var options = make(map[int]int)

func dirac(player1ToPlay bool, score1, score2, p1, p2 int, visited map[Key]Value, universe int) (int, int) {
	k := Key{player1ToPlay, score1, score2, p1, p2, universe}
	if v, exists := visited[k]; exists {
		return v.p1, v.p2
	}

	sumP1 := 0
	sumP2 := 0

	for dice := 3; dice <= 9; dice++ {
		const win = 21
		if player1ToPlay {
			p := (p1 + dice) % 10
			score := score1 + p + 1
			if score >= win {
				sumP1 += universe * options[dice]
				continue
			}
			a, b := dirac(!player1ToPlay, score, score2, p, p2, visited, universe*options[dice])
			sumP1 += a
			sumP2 += b
		} else {
			p := (p2 + dice) % 10
			score := score2 + p + 1
			if score >= win {
				sumP2 += universe * options[dice]
				continue
			}
			a, b := dirac(!player1ToPlay, score1, score, p1, p, visited, universe*options[dice])
			sumP1 += a
			sumP2 += b
		}
	}
	visited[k] = Value{sumP1, sumP2}
	return sumP1, sumP2
}

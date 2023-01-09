package main

import (
	"bufio"
	"io"
	"math"
)

/*
at least one damage
if can't afford to cast a spell => loss
least amount of mana to still win a fight
*/
func fs1(playerHitPoints, playerMana, bossHitPoints, bossDamage int, abilities map[string]Spell) int {
	player := Player{
		hitPoints: playerHitPoints,
		mana:      playerMana,
	}
	boss := Boss{
		hitPoints: bossHitPoints,
		damage:    bossDamage,
	}

	return game(player, boss, abilities, make(map[string]Spell), 0)
}

func copySpells(s []Spell) []Spell {
	res := make([]Spell, len(s), len(s)+1)
	for i, s := range s {
		res[i] = s
	}
	return res
}

func newSpell(spells []Spell, spell Spell) []Spell {
	res := make([]Spell, len(spells), len(spells)+1)
	for i, s := range spells {
		res[i] = s
	}
	res = append(res, spell)
	return res
}

func newTurnSpells(currentSpells map[string]Spell) map[string]Spell {
	spells := make(map[string]Spell)
	for k, spell := range currentSpells {
		spell.last--
		if spell.last > 0 {
			spells[k] = spell
		}
	}
	return spells
}

type Boss struct {
	hitPoints int
	damage    int
}

func (b Boss) apply(spell Spell) Boss {
	b.hitPoints -= spell.damage
	return b
}

func (b Boss) attack(player Player) Player {
	d := damage(b.damage, player.armor)
	player.hitPoints -= d
	return player
}

func (b Boss) isDead() bool {
	return b.hitPoints <= 0
}

type Player struct {
	hitPoints int
	mana      int
	armor     int
}

func (p Player) castable(spell Spell) bool {
	return spell.cost <= p.mana
}

func (p Player) apply(passive bool, spell Spell) Player {
	p.hitPoints += spell.heal
	p.armor = spell.armorIncreased
	p.mana += spell.manaRecharge
	if !passive {
		p.mana -= spell.cost
	}
	return p
}

func (p Player) isDead() bool {
	return p.hitPoints <= 0
}

func passiveSpells(player Player, boss Boss, spells map[string]Spell) (Player, Boss, map[string]Spell) {
	res := make(map[string]Spell)
	for k, spell := range spells {
		player = player.apply(true, spell)
		boss = boss.apply(spell)
		spell.last--
		if spell.last > 0 {
			res[k] = spell
		}
	}
	return player, boss, res
}

func game(player Player, boss Boss, abilities map[string]Spell, spells map[string]Spell, manaSpent int) int {
	// Player turn

	// Passive
	player, boss, spells = passiveSpells(player, boss, spells)
	if boss.isDead() {
		return manaSpent
	}

	// Cast
	minMana := math.MaxInt
	for k, spell := range abilities {
		player := player
		boss := boss

		if _, exists := spells[k]; exists {
			continue
		}

		manaSpent := manaSpent
		manaSpent += spell.cost

		if spell.active() {
			if !player.castable(spell) {
				continue
			}
			player = player.apply(false, spell)
			boss = boss.apply(spell)
			if boss.isDead() {
				if manaSpent < minMana {
					minMana = manaSpent
				}
			}
		} else {
			if !player.castable(spell) {
				continue
			}
			player.mana -= spell.cost
			spells[k] = spell
		}

		// Boss turn

		// Passive
		player, boss, spells = passiveSpells(player, boss, spells)
		if boss.isDead() {
			minMana = manaSpent
		}

		// Attack
		player = boss.attack(player)
		if player.isDead() {
			continue
		}

		if v := game(player, boss, abilities, spells, manaSpent); v < minMana {
			minMana = v
		}

		delete(spells, k)
	}

	return minMana
}

func gamey(playerHitPoints, playerMana, bossHitPoints, bossDamage int, abilities map[string]Spell, currentSpells map[string]Spell, manaSpent int, list []Spell) (int, []Spell) {
	// New turn
	spells := newTurnSpells(currentSpells)
	armor := 0
	for _, spell := range spells {
		armor += spell.armorIncreased
		bossHitPoints -= spell.damage
		playerMana += spell.manaRecharge
	}
	if bossHitPoints <= 0 {
		return manaSpent, list
	}

	// Chose spell
	min := math.MaxInt
	var bestSpells []Spell
	for k, spell := range abilities {
		if _, exists := spells[k]; exists {
			continue
		}

		bossHitPoints := bossHitPoints
		playerHitPoints := playerHitPoints
		armor := armor
		playerMana := playerMana
		manaSpent := manaSpent

		manaSpent += spell.cost
		playerMana -= spell.cost

		if spell.last == 0 {
			bossHitPoints -= spell.damage
			if bossHitPoints <= 0 {
				if manaSpent < min {
					min = manaSpent
					bestSpells = newSpell(list, spell)
				}
				continue
			}

			playerHitPoints += spell.heal
		} else {
			armor += spell.armorIncreased
			playerMana += spell.manaRecharge
			spells[k] = spell
		}

		// New turn
		spells := newTurnSpells(spells)
		for _, spell := range spells {
			bossHitPoints -= spell.damage
			playerMana += spell.manaRecharge
		}
		if bossHitPoints <= 0 {
			if manaSpent < min {
				min = manaSpent
				bestSpells = newSpell(list, spell)
			}
			delete(spells, k)
			continue
		}

		d := damage(bossDamage, armor)
		playerHitPoints -= d
		if playerHitPoints <= d {
			delete(spells, k)
			continue
		}

		v, v2 := gamey(playerHitPoints, playerMana, bossHitPoints, bossDamage, abilities, spells, manaSpent, newSpell(list, spell))
		if v < min {
			bestSpells = v2
		}
		delete(spells, k)
	}

	return min, bestSpells
}

func gamex(playerHitPoints, playerMana, bossHitPoints, bossDamage int, abilities map[string]Spell, currentSpells map[string]Spell, manaSpent int, list []Spell) (int, []Spell) {
	// Remove outdated spells
	spells := newTurnSpells(currentSpells)
	armor := 0
	for _, spell := range spells {
		armor += spell.armorIncreased
		bossHitPoints -= spell.damage
		playerMana += spell.manaRecharge
	}
	if bossHitPoints <= 0 {
		return manaSpent, list
	}

	// Player
	min := math.MaxInt
	var bestSpells []Spell
	for k, spell := range abilities {
		if spell.cost > playerMana {
			continue
		}

		if spell.last == 0 {
			if bossHitPoints-spell.damage <= 0 {
				min = getMin(min, manaSpent+spell.cost)
				continue
			}

			// Boss
			spells = newTurnSpells(spells)
			for _, s := range spells {
				armor += s.armorIncreased
				bossHitPoints -= s.damage
				playerMana += s.manaRecharge
			}
			if bossHitPoints <= 0 {
				return manaSpent, list
			}

			d := damage(bossDamage, armor)
			if playerHitPoints-d+spell.heal <= 0 {
				continue
			}

			v, sp := gamex(playerHitPoints-d+spell.heal, playerMana-spell.cost, bossHitPoints-spell.damage, bossDamage, abilities, spells, manaSpent+spell.cost, append(copySpells(list), spell))
			if v < min {
				min = v
				bestSpells = sp
			}
		} else {
			spells[k] = spell
			if bossHitPoints-spell.damage <= 0 {
				min = getMin(min, manaSpent+spell.cost)
				continue
			}

			// Boss
			spells = newTurnSpells(spells)
			for _, s := range spells {
				armor += s.armorIncreased
				bossHitPoints -= s.damage
				playerMana += s.manaRecharge
			}
			if bossHitPoints <= 0 {
				return manaSpent, list
			}

			d := damage(bossDamage, armor+spell.armorIncreased)
			if playerHitPoints-d <= 0 {
				continue
			}

			v, sp := gamex(playerHitPoints-d, playerMana+spell.manaRecharge, bossHitPoints-spell.damage, bossDamage, abilities, spells, manaSpent+spell.cost, append(copySpells(list), spell))
			if v < min {
				min = v
				bestSpells = sp
			}
			delete(spells, k)
		}
	}

	return min, bestSpells
}

func damage(bossDamage, armor int) int {
	v := bossDamage - armor
	if v < 1 {
		return 1
	}
	return v
}

func getMin(a, b int) int {
	if a < b {
		return a
	}
	return b
}

type Spell struct {
	cost           int
	damage         int
	heal           int
	armorIncreased int
	last           int
	manaRecharge   int
}

func (s Spell) active() bool {
	return s.last == 0
}

func fs2(input io.Reader) (int, error) {
	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
		line := scanner.Text()
		_ = line
	}

	return 0, nil
}

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
	return game(playerHitPoints, playerMana, bossHitPoints, bossDamage, abilities, make(map[string]Spell), 0)
}

func copySpells(s []Spell) []Spell {
	res := make([]Spell, len(s))
	for i, s := range s {
		res[i] = s
	}
	return res
}

func game(playerHitPoints, playerMana, bossHitPoints, bossDamage int, abilities map[string]Spell, currentSpells map[string]Spell, manaSpent int) int {
	// Remove outdated spells
	spells := make(map[string]Spell)
	for k, spell := range currentSpells {
		spell.last--
		if spell.last > 0 {
			spells[k] = spell
		}
	}

	armor := 0
	// Run all the current spells
	for _, spell := range spells {
		armor += spell.armorIncreased
		bossHitPoints -= spell.damage
		playerMana += spell.manaRecharge
	}

	if bossHitPoints <= 0 {
		return manaSpent
	}

	// Player
	min := math.MaxInt
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
			d := damage(bossDamage, armor)
			if playerHitPoints-d+spell.heal <= 0 {
				continue
			}

			min = getMin(min, game(playerHitPoints-d+spell.heal, playerMana-spell.cost, bossHitPoints-spell.damage, bossDamage, abilities, spells, manaSpent+spell.cost))
		} else {
			spells[k] = spell
			if bossHitPoints-spell.damage <= 0 {
				min = getMin(min, manaSpent+spell.cost)
				continue
			}

			// Boss
			d := damage(bossDamage, armor+spell.armorIncreased)
			if playerHitPoints-d <= 0 {
				continue
			}

			min = getMin(min, game(playerHitPoints-d, playerMana+spell.manaRecharge, bossHitPoints-spell.damage, bossDamage, abilities, spells, manaSpent+spell.cost))
			delete(spells, k)
		}
	}

	return min
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

func fs2(input io.Reader) (int, error) {
	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
		line := scanner.Text()
		_ = line
	}

	return 0, nil
}

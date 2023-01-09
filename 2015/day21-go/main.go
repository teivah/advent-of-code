package main

import (
	"math"
)

type Unit struct {
	cost   int
	damage int
	armor  int
}

/*
damage = attacker's damage - defender's armor (at least 1)

damage = 0
armor = 0

total damage or armor = sum of all the items
100 hit points

1 weapon
0-1 armor
0-2 rings

least amount of gold
*/
func fs1(bossHitPoints, bossDamage, bossArmor int, storeWeapons []Unit, storeArmors []Unit, storeRings []Unit) int {
	var rings [2]*Unit

	minGold := math.MaxInt
	for _, w := range storeWeapons {
		// With armor
		for _, a := range storeArmors {
			minGold = min(minGold, round(100, w, &a, rings, bossHitPoints, bossDamage, bossArmor, storeRings, w.cost+a.cost, 0))
		}
		// Without armor
		minGold = min(minGold, round(100, w, nil, rings, bossHitPoints, bossDamage, bossArmor, storeRings, w.cost, 0))
	}

	return minGold
}

func round(playerHitPoints int, weapon Unit, armor *Unit, rings [2]*Unit, bossHitPoints, bossDamage, bossArmor int, storeRings []Unit, gold int, storeIndex int) int {
	if storeIndex == len(storeRings) {
		return math.MaxInt
	}

	curRings := 0
	if rings[0] == nil {
		curRings = 0
	} else if rings[1] == nil {
		curRings = 1
	} else {
		curRings = 2
	}

	// With
	with := math.MaxInt
	if curRings < 2 {
		rings[curRings] = &storeRings[storeIndex]
		playerDamage, playerArmor := sum(weapon, armor, rings)
		if win(playerHitPoints, playerDamage, playerArmor, bossHitPoints, bossDamage, bossArmor) {
			with = gold + storeRings[storeIndex].cost
		} else {
			with = round(playerHitPoints, weapon, armor, rings, bossHitPoints, bossDamage, bossArmor, storeRings, gold+storeRings[storeIndex].cost, storeIndex+1)
		}
		rings[curRings] = nil
	}

	// Without
	without := round(playerHitPoints, weapon, armor, rings, bossHitPoints, bossDamage, bossArmor, storeRings, gold, storeIndex+1)

	return min(with, without)
}

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func sum(weapon Unit, armor *Unit, rings [2]*Unit) (int, int) {
	sumDamage := 0
	sumArmor := 0

	sumDamage += weapon.damage
	sumArmor += weapon.armor
	if armor != nil {
		sumDamage += armor.damage
		sumArmor += armor.armor
	}
	for _, ring := range rings {
		if ring != nil {
			sumDamage += ring.damage
			sumArmor += ring.armor
		}
	}

	return sumDamage, sumArmor
}

func win(playerHitPoints, playerDamage, playerArmor, bossHitPoints, bossDamage, bossArmor int) bool {
	for {
		// Player
		d := calcDamage(playerDamage, bossArmor)
		bossHitPoints -= d
		if bossHitPoints <= 0 {
			return true
		}

		// Boss
		d = calcDamage(bossDamage, playerArmor)
		playerHitPoints -= d
		if playerHitPoints <= 0 {
			return false
		}
	}
}

func calcDamage(damage, armor int) int {
	v := damage - armor
	if v < 1 {
		return 1
	}
	return v
}

func fs2(bossHitPoints, bossDamage, bossArmor int, storeWeapons []Unit, storeArmors []Unit, storeRings []Unit) int {
	return 0
}

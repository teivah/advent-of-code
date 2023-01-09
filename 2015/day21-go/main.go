package main

import (
	"fmt"
	"math"
	"strings"
)

type Unit struct {
	cost   int
	damage int
	armor  int
}

func fs1(bossHitPoints, bossDamage, bossArmor int, storeWeapons []Unit, storeArmors []Unit, storeRings []Unit) int {
	equipments := allEquipments(storeWeapons, storeArmors, storeRings)

	minGold := math.MaxInt
	for _, equipment := range equipments {
		playerDamage, playerArmor, gold := equipment.sum(0, 0)

		if win(100, playerDamage, playerArmor, bossHitPoints, bossDamage, bossArmor) {
			minGold = min(minGold, gold)
		}
	}

	return minGold
}

type Equipment struct {
	weapon Unit
	armor  *Unit
	ring1  *Unit
	ring2  *Unit
}

func (e Equipment) toString() string {
	wb := strings.Builder{}
	wb.WriteString(fmt.Sprintf("w=%v ", e.weapon.cost))
	if e.armor != nil {
		wb.WriteString(fmt.Sprintf(" a=%v", e.armor.cost))
	}
	if e.ring1 != nil {
		wb.WriteString(fmt.Sprintf(" r1=%v", e.ring1.cost))
	}
	if e.ring2 != nil {
		wb.WriteString(fmt.Sprintf(" r2=%v", e.ring2.cost))
	}
	return wb.String()
}

func (e Equipment) sum(sumDamage int, sumArmor int) (int, int, int) {
	gold := 0

	sumDamage += e.weapon.damage
	sumArmor += e.weapon.armor
	gold = e.weapon.cost

	if e.armor != nil {
		sumDamage += e.armor.damage
		sumArmor += e.armor.armor
		gold += e.armor.cost
	}
	if e.ring1 != nil {
		sumDamage += e.ring1.damage
		sumArmor += e.ring1.armor
		gold += e.ring1.cost
	}
	if e.ring2 != nil {
		sumDamage += e.ring2.damage
		sumArmor += e.ring2.armor
		gold += e.ring2.cost
	}

	return sumDamage, sumArmor, gold
}

func allEquipments(storeWeapons []Unit, storeArmors []Unit, storeRings []Unit) []Equipment {
	var equipments []Equipment

	for _, w := range storeWeapons {
		// With armor
		for _, a := range storeArmors {
			a := a
			equipments = append(equipments, all(w, &a, nil, nil, storeRings, 0)...)
		}
		equipments = append(equipments, all(w, nil, nil, nil, storeRings, 0)...)
	}

	return equipments
}

func all(weapon Unit, armor, ring1, ring2 *Unit, storeRings []Unit, storeIndex int) []Equipment {
	if storeIndex == len(storeRings) {
		return nil
	}

	if ring1 != nil && ring2 != nil {
		return nil
	}

	var equipments []Equipment

	// With
	if ring1 == nil {
		equipments = append(equipments, Equipment{
			weapon: weapon,
			armor:  armor,
			ring1:  &storeRings[storeIndex],
		})
		equipments = append(equipments, all(weapon, armor, &storeRings[storeIndex], nil, storeRings, storeIndex+1)...)
	} else {
		equipments = append(equipments, Equipment{
			weapon: weapon,
			armor:  armor,
			ring1:  ring1,
			ring2:  &storeRings[storeIndex],
		})
		equipments = append(equipments, all(weapon, armor, ring1, &storeRings[storeIndex], storeRings, storeIndex+1)...)
	}

	// Without
	equipments = append(equipments, all(weapon, armor, ring1, ring2, storeRings, storeIndex+1)...)

	return equipments
}

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func win(playerHitPoints, playerDamage, playerArmor, bossHitPoints, bossDamage, bossArmor int) bool {
	pd := calcDamage(playerDamage, bossArmor)
	bd := calcDamage(bossDamage, playerArmor)

	for {
		// Player
		bossHitPoints -= pd
		if bossHitPoints <= 0 {
			return true
		}

		// Boss
		playerHitPoints -= bd
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
	equipments := allEquipments(storeWeapons, storeArmors, storeRings)

	maxGold := 0
	for _, equipment := range equipments {
		playerDamage, playerArmor, gold := equipment.sum(0, 0)

		if !win(100, playerDamage, playerArmor, bossHitPoints, bossDamage, bossArmor) {
			maxGold = max(maxGold, gold)
		}
	}

	return maxGold
}

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

package main

import (
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

	cache = make(map[Player]map[Boss]map[int]struct{})

	return game1(player, boss, abilities, nil, 0)
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
	p.armor += spell.armorIncreased
	p.mana += spell.manaRecharge
	if !passive {
		p.mana -= spell.cost
	}
	return p
}

func (p Player) isDead() bool {
	return p.hitPoints <= 0
}

func passiveSpells(player Player, boss Boss, spells []Spell) (Player, Boss, []Spell) {
	res := make([]Spell, 0, len(spells))
	for _, spell := range spells {
		player = player.apply(true, spell)
		boss = boss.apply(spell)
		spell.last--
		if spell.last > 0 {
			res = append(res, spell)
		}
	}
	player.hitPoints--
	return player, boss, res
}

func addSpell(spells []Spell, spell Spell) []Spell {
	res := make([]Spell, len(spells), len(spells)+1)
	for i, s := range spells {
		res[i] = s
	}
	res = append(res, spell)
	return res
}

func copySpells(spells []Spell) []Spell {
	res := make([]Spell, len(spells))
	for i, s := range spells {
		res[i] = s
	}
	return res
}

func deleteSpell(spells []Spell) []Spell {
	return spells[:len(spells)-1]
}

var cache map[Player]map[Boss]map[int]struct{}

func addCache(player Player, boss Boss, manaSpent int) {
	v, exists := cache[player]
	if !exists {
		v = make(map[Boss]map[int]struct{})
		cache[player] = v
	}

	v2, exists := v[boss]
	if !exists {
		v2 = make(map[int]struct{})
		v[boss] = v2
	}

	v2[manaSpent] = struct{}{}
}

func containsCache(player Player, boss Boss, manaSpent int) bool {
	v, exists := cache[player]
	if !exists {
		return false
	}

	v2, exists := v[boss]
	if !exists {
		return false
	}

	_, exists = v2[manaSpent]
	return exists
}

func game1(player Player, boss Boss, abilities map[string]Spell, spells []Spell, manaSpent int) int {
	if containsCache(player, boss, manaSpent) {
		return math.MaxInt
	}

	addCache(player, boss, manaSpent)

	// Player turn
	player.armor = 0

	// Passive
	player, boss, spells = passiveSpells(player, boss, spells)
	if boss.isDead() {
		return manaSpent
	}

	// Cast
	minMana := math.MaxInt
	for k, spell := range abilities {
		spells := copySpells(spells)
		player := player
		boss := boss
		_ = k

		if spell.exists(spells) {
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
			spells = append(spells, spell)
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

		if v := game1(player, boss, abilities, spells, manaSpent); v < minMana {
			minMana = v
		}
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

func (s Spell) exists(spells []Spell) bool {
	for _, spell := range spells {
		if s.cost == spell.cost {
			return true
		}
	}
	return false
}

func (s Spell) active() bool {
	return s.last == 0
}

func fs2(playerHitPoints, playerMana, bossHitPoints, bossDamage int, abilities map[string]Spell) int {
	player := Player{
		hitPoints: playerHitPoints,
		mana:      playerMana,
	}
	boss := Boss{
		hitPoints: bossHitPoints,
		damage:    bossDamage,
	}

	cache = make(map[Player]map[Boss]map[int]struct{})

	return game2(player, boss, abilities, nil, 0)
}

func game2(player Player, boss Boss, abilities map[string]Spell, spells []Spell, manaSpent int) int {
	if containsCache(player, boss, manaSpent) {
		return math.MaxInt
	}

	addCache(player, boss, manaSpent)

	// Player turn
	player.armor = 0

	// Passive
	player, boss, spells = passiveSpells(player, boss, spells)
	if boss.isDead() {
		return manaSpent
	}
	if player.isDead() {
		return math.MaxInt
	}

	// Cast
	minMana := math.MaxInt
	for k, spell := range abilities {
		spells := copySpells(spells)
		player := player
		boss := boss
		_ = k

		if spell.exists(spells) {
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
			spells = append(spells, spell)
		}

		// Boss turn

		// Passive
		player, boss, spells = passiveSpells(player, boss, spells)
		if boss.isDead() {
			minMana = manaSpent
		}
		if player.isDead() {
			return math.MaxInt
		}

		// Attack
		player = boss.attack(player)
		if player.isDead() {
			continue
		}

		if v := game1(player, boss, abilities, spells, manaSpent); v < minMana {
			minMana = v
		}
	}

	return minMana
}

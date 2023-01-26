package main

import (
	"fmt"
	"io"
	"math"
	"sort"
	"strings"

	lib "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader, boost int) (int, bool) {
	return fs(lib.ReaderToStrings(input), boost)
}

func fs(lines []string, boost int) (int, bool) {
	immunes, infections := parse(lines, boost)
	isImmuneArmy := make(map[string]bool)
	for k := range immunes {
		isImmuneArmy[k] = true
	}
	for k := range infections {
		isImmuneArmy[k] = false
	}

game:
	for {
		var groups []Group
		for _, group := range immunes {
			groups = append(groups, group)
		}
		for _, group := range infections {
			groups = append(groups, group)
		}

		// Sort for selection
		sort.Slice(groups, func(i, j int) bool {
			a := groups[i]
			b := groups[j]
			if a.effectivePower() > b.effectivePower() {
				return true
			}
			if b.effectivePower() > a.effectivePower() {
				return false
			}
			return a.initiative > b.initiative
		})

		remainingImmuneArmy := make(map[string]struct{})
		for k := range immunes {
			remainingImmuneArmy[k] = struct{}{}
		}

		remainingInfectionArmy := make(map[string]struct{})
		for k := range infections {
			remainingInfectionArmy[k] = struct{}{}
		}

		// Do selections
		selections := make(map[string]string)
		for _, attacker := range groups {
			target := ""
			maxDamage := -1
			targetInitiative := -1
			targetEffectivePower := -1
			var enemyGroup map[string]Group
			if attacker.immuneArmy {
				enemyGroup = infections
			} else {
				enemyGroup = immunes
			}
			for _, defender := range enemyGroup {
				if attacker.immuneArmy {
					if _, exists := remainingInfectionArmy[defender.id]; !exists {
						continue
					}
				} else {
					if _, exists := remainingImmuneArmy[defender.id]; !exists {
						continue
					}
				}

				damage := attacker.calculateDamagePoints(&defender)
				//fmt.Printf("%s would deal defending %s %d damage\n", attacker.id, defender.id, damage)
				if damage == 0 {
					continue
				}
				if damage > maxDamage ||
					(damage == maxDamage && defender.effectivePower() > targetEffectivePower) ||
					(damage == maxDamage && defender.effectivePower() == targetEffectivePower && defender.initiative > targetInitiative) {
					maxDamage = damage
					target = defender.id
					targetInitiative = defender.initiative
					targetEffectivePower = defender.effectivePower()
				}
			}

			if target != "" {
				selections[attacker.id] = target

				if isImmuneArmy[target] {
					delete(remainingImmuneArmy, target)
				} else {
					delete(remainingInfectionArmy, target)
				}
			}
		}

		// Attack order
		var attackers []string
		for attacker := range selections {
			attackers = append(attackers, attacker)
		}
		sort.Slice(attackers, func(i, j int) bool {
			a := attackers[i]
			b := attackers[j]

			ia := 0
			ib := 0

			if isImmuneArmy[a] {
				ia = immunes[a].initiative
			} else {
				ia = infections[a].initiative
			}

			if isImmuneArmy[b] {
				ib = immunes[b].initiative
			} else {
				ib = infections[b].initiative
			}

			return ia > ib
		})

		// Attack
		sumDead := 0
		for _, id := range attackers {
			attacker := getGroup(id, isImmuneArmy, immunes, infections)
			defender := getGroup(selections[id], isImmuneArmy, immunes, infections)

			deadUnits := attacker.attack(defender)
			sumDead += deadUnits

			if isImmuneArmy[defender.id] {
				v := immunes[defender.id]
				v.units -= deadUnits
				immunes[defender.id] = v
				if v.units == 0 {
					delete(immunes, defender.id)
					if len(immunes) == 0 {
						break game
					}
				}
			} else {
				v := infections[defender.id]
				v.units -= deadUnits
				infections[defender.id] = v
				if v.units == 0 {
					delete(infections, defender.id)
					if len(infections) == 0 {
						break game
					}
				}
			}
		}

		// Is draw?
		if sumDead == 0 {
			return 0, false
		}
	}

	sum := 0
	immuneWin := false
	if len(immunes) != 0 {
		for _, group := range immunes {
			sum += group.units
		}
		immuneWin = true
	} else {
		for _, group := range infections {
			sum += group.units
		}
	}

	return sum, immuneWin
}

func getGroup(id string, isImmuneArmy map[string]bool, immunes map[string]Group, infections map[string]Group) *Group {
	if isImmuneArmy[id] {
		group := immunes[id]
		return &group
	}
	group := infections[id]
	return &group
}

func parse(lines []string, boost int) (map[string]Group, map[string]Group) {
	curGroup := make(map[string]Group)
	var a map[string]Group

	immuneArmy := true
	id := 1
	for i := 1; i < len(lines); i++ {
		line := lines[i]
		if line == "" {
			a = curGroup
			curGroup = make(map[string]Group)
			immuneArmy = !immuneArmy
			i++
			id = 1
			continue
		}

		group := toGroup(line)
		if immuneArmy {
			group.id = fmt.Sprintf("immune group %d", id)
			group.damage += boost
		} else {
			group.id = fmt.Sprintf("infection group %d", id)
		}
		group.immuneArmy = immuneArmy
		curGroup[group.id] = group
		id++
	}

	return a, curGroup
}

func toGroup(s string) Group {
	spaces := lib.NewDelimiter(s, " ")

	a := strings.Index(s, "immune to")
	immuneTo := make(map[string]struct{})
	if a != -1 {
		sep := strings.Index(s[a:], ";")
		if sep == -1 {
			sep = math.MaxInt
		}
		par := strings.Index(s[a:], ")")
		if par == -1 {
			par = math.MaxInt
		}
		idx := lib.Min(sep, par)

		v := s[a+len("immune to")+1 : a+idx]
		split := strings.Split(v, ", ")
		for _, v := range split {
			immuneTo[v] = struct{}{}
		}
	}

	a = strings.Index(s, "weak to")
	weakTo := make(map[string]struct{})
	if a != -1 {
		sep := strings.Index(s[a:], ";")
		if sep == -1 {
			sep = math.MaxInt
		}
		par := strings.Index(s[a:], ")")
		if par == -1 {
			par = math.MaxInt
		}
		idx := lib.Min(sep, par)

		v := s[a+len("weak to")+1 : a+idx]
		split := strings.Split(v, ", ")
		for _, v := range split {
			weakTo[v] = struct{}{}
		}
	}

	idx := strings.Index(s, "that does ")
	v := s[idx+len("that does")+1:]
	space := strings.Index(v, " ")
	damage := lib.StringToInt(v[:space])
	del := lib.NewDelimiter(v, " ")
	return Group{
		units:      spaces.GetInt(0),
		hitPoints:  spaces.GetInt(4),
		immuneTo:   immuneTo,
		weakTo:     weakTo,
		damage:     damage,
		attackType: del.GetString(1),
		initiative: del.GetInt(5),
	}
}

type Group struct {
	id         string
	immuneArmy bool
	units      int
	hitPoints  int
	immuneTo   map[string]struct{}
	weakTo     map[string]struct{}
	damage     int
	attackType string
	initiative int
}

func totalUnits(groups map[string]Group) int {
	sum := 0
	for _, group := range groups {
		sum += group.units
	}
	return sum
}

func (g *Group) targetSelection() int {
	return 0
}

func (g *Group) effectivePower() int {
	return g.units * g.damage
}

func (g *Group) calculateDamagePoints(enemy *Group) int {
	if _, exists := enemy.immuneTo[g.attackType]; exists {
		return 0
	}

	if _, exists := enemy.weakTo[g.attackType]; exists {
		return g.effectivePower() * 2
	}

	return g.effectivePower()
}

func (g *Group) attack(enemy *Group) int {
	damage := g.calculateDamagePoints(enemy)
	if damage == 0 {
		return 0
	}

	units := enemy.units
	killed := 0
	for i := 0; i < units; i++ {
		if enemy.hitPoints > damage {
			break
		}
		enemy.units--
		killed++
		if enemy.units == 0 {
			break
		}
		damage -= enemy.hitPoints
	}

	return killed
}

func fs2(input io.Reader) int {
	lines := lib.ReaderToStrings(input)
	for boost := 3; ; boost++ {
		if remaining, immuneWin := fs(lines, boost); immuneWin {
			return remaining
		}
	}
}

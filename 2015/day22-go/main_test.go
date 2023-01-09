package main

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

// poison
// magic missile
func TestUnit(t *testing.T) {
	v := fs1(10, 250, 13, 8, map[string]Spell{
		"missile":  {cost: 53, last: 0, damage: 4},
		"drain":    {cost: 73, last: 0, damage: 2, heal: 2},
		"shield":   {cost: 113, last: 6, armorIncreased: 7},
		"poison":   {cost: 173, last: 6, damage: 3},
		"recharge": {cost: 229, last: 5, manaRecharge: 101},
	})
	assert.Equal(t, 226, v)
}

// recharge
// shield
// drain
// poison
// missile
func TestUnit2(t *testing.T) {
	v := fs1(10, 250, 14, 8, map[string]Spell{
		"missile":  {cost: 53, last: 0, damage: 4},
		"drain":    {cost: 73, last: 0, damage: 2, heal: 2},
		"shield":   {cost: 113, last: 6, armorIncreased: 7},
		"poison":   {cost: 173, last: 6, damage: 3},
		"recharge": {cost: 229, last: 5, manaRecharge: 101},
	})
	assert.Equal(t, 641, v)
}

func TestFs1Input(t *testing.T) {
	v := fs1(50, 500, 58, 9, map[string]Spell{
		"missile":  {cost: 53, last: 0, damage: 4},
		"drain":    {cost: 73, last: 0, damage: 2, heal: 2},
		"shield":   {cost: 113, last: 6, armorIncreased: 7},
		"poison":   {cost: 173, last: 6, damage: 3},
		"recharge": {cost: 229, last: 5, manaRecharge: 101},
	})
	assert.Equal(t, 1269, v)
}

func TestFs2Input(t *testing.T) {
	v := fs2(50, 500, 58, 9, map[string]Spell{
		"missile":  {cost: 53, last: 0, damage: 4},
		"drain":    {cost: 73, last: 0, damage: 2, heal: 2},
		"shield":   {cost: 113, last: 6, armorIncreased: 7},
		"poison":   {cost: 173, last: 6, damage: 3},
		"recharge": {cost: 229, last: 5, manaRecharge: 101},
	})
	assert.Equal(t, 1269, v)
}

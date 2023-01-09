package main

import (
	"os"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestFs1Input(t *testing.T) {
	v := fs1(50, 500, 58, 9, map[string]Spell{
		"missile":  {cost: 53, last: 0, damage: 4},
		"drain":    {cost: 73, last: 0, damage: 2, heal: 2},
		"shield":   {cost: 113, last: 6, armorIncreased: 7},
		"poison":   {cost: 173, last: 6, damage: 3},
		"recharge": {cost: 229, last: 5, manaRecharge: 101},
	})
	assert.Equal(t, 1002, v)
}

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

func TestFs2Test(t *testing.T) {
	f, err := os.Open("test.txt")
	require.NoError(t, err)
	v, err := fs2(f)
	require.NoError(t, err)
	assert.Equal(t, 42, v)
}

func TestFs2Input(t *testing.T) {
	f, err := os.Open("input.txt")
	require.NoError(t, err)
	v, err := fs2(f)
	require.NoError(t, err)
	assert.Equal(t, 42, v)
}

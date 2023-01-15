package main

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestDoors(t *testing.T) {
	assert.Equal(t, Doors{
		up:    true,
		down:  true,
		left:  true,
		right: false,
	}, doors("hijkl", ""))
}

func TestFs1Test(t *testing.T) {
	assert.Equal(t, "DDRRRD", fs1("ihgpwlah"))
	assert.Equal(t, "DDUDRLRRUDRD", fs1("kglvqrro"))
	assert.Equal(t, "DRURDRUDDLLDLUURRDULRLDUUDDDRR", fs1("ulqzkmiv"))
}

func TestFs1Input(t *testing.T) {
	assert.Equal(t, "RLDRUDRDDR", fs1("yjjvjgan"))
}

func TestFs2Test(t *testing.T) {
	assert.Equal(t, 370, fs2("ihgpwlah"))
	assert.Equal(t, 492, fs2("kglvqrro"))
	assert.Equal(t, 830, fs2("ulqzkmiv"))
}

func TestFs2Input(t *testing.T) {
	assert.Equal(t, 498, fs2("yjjvjgan"))
}

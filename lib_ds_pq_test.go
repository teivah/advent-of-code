package aoc_test

import (
	"testing"

	"github.com/stretchr/testify/assert"
	aoc "github.com/teivah/advent-of-code"
)

func TestNewPriorityQueue(t *testing.T) {
	type person struct {
		age int
	}
	pq := aoc.NewPriorityQueue[person](func(a, b person) bool {
		return a.age < b.age
	})
	pq.Push(person{age: 2})
	pq.Push(person{age: 1})

	assert.Equal(t, 2, pq.Len())
	assert.Equal(t, false, pq.IsEmpty())

	p := pq.Pop()
	assert.Equal(t, 1, p.age)

	p = pq.Peek()
	assert.Equal(t, 2, p.age)

	p = pq.Pop()
	assert.Equal(t, 2, p.age)

	assert.Equal(t, true, pq.IsEmpty())
}

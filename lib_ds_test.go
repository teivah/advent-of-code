package aoc_test

import (
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	aoc "github.com/teivah/advent-of-code"
)

func TestNewPriorityQueue(t *testing.T) {
	type person struct {
		age int
	}
	pq := aoc.NewPriorityQueue[person](func(a, b person) int {
		return a.age - b.age
	})
	pq.Push(person{age: 2})
	pq.Push(person{age: 1})

	assert.Equal(t, 2, pq.Len())
	assert.Equal(t, false, pq.IsEmpty())

	p, contains := pq.Pop()
	require.True(t, contains)
	assert.Equal(t, 1, p.age)

	p, contains = pq.Peek()
	require.True(t, contains)
	assert.Equal(t, 2, p.age)

	p, contains = pq.Pop()
	require.True(t, contains)
	assert.Equal(t, 2, p.age)

	_, contains = pq.Pop()
	require.False(t, contains)

	assert.Equal(t, true, pq.IsEmpty())
}

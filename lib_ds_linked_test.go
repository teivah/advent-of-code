package aoc

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestNode(t *testing.T) {
	head := NewNode[int](3)
	assert.Equal(t, "3", head.String())

	head = head.InsertHead(2)
	assert.Equal(t, "2,3", head.String())

	tail := head.InsertTail(4)
	assert.Equal(t, "2,3,4", head.String())
	assert.Equal(t, "4", tail.String())

	head = tail.InsertHead(1)
	assert.Equal(t, "1,2,3,4", head.String())

	head.Next.InsertAfter(10)
	assert.Equal(t, "1,2,10,3,4", head.String())
}

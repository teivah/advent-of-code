package aoc

import (
	"fmt"
	"strings"
)

// Node is a linked list node.
type Node[T any] struct {
	Data     T
	Previous *Node[T]
	Next     *Node[T]
}

// NewNode returns a new node.
func NewNode[T any](data T) *Node[T] {
	return &Node[T]{Data: data}
}

// Tail returns the tail.
func (n *Node[T]) Tail() *Node[T] {
	cur := n
	for cur.Next != nil {
		cur = cur.Next
	}
	return cur
}

// Head returns the head.
func (n *Node[T]) Head() *Node[T] {
	cur := n
	for cur.Previous != nil {
		cur = cur.Previous
	}
	return cur
}

// InsertAfter insers a node after the current one.
func (n *Node[T]) InsertAfter(data T) {
	node := &Node[T]{
		Data:     data,
		Previous: n,
	}
	if n.Next != nil {
		next := n.Next
		next.Previous = node
		node.Next = next
	}
	n.Next = node
}

// InsertHead inserts a node to the head and return the head.
func (n *Node[T]) InsertHead(data T) *Node[T] {
	head := n.Head()
	node := &Node[T]{
		Data: data,
		Next: head,
	}
	head.Previous = node
	return node
}

// InsertTail inserts a node to the tail and return the tail.
func (n *Node[T]) InsertTail(data T) *Node[T] {
	tail := n.Tail()
	node := &Node[T]{
		Data:     data,
		Previous: tail,
	}
	tail.Next = node
	return node
}

// String implements strings.Stringer.
func (n *Node[T]) String() string {
	var s []string
	cur := n
	for cur != nil {
		s = append(s, fmt.Sprintf("%v", cur.Data))
		cur = cur.Next
	}
	return strings.Join(s, ",")
}

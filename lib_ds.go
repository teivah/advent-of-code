package aoc

import (
	pq "github.com/emirpasic/gods/queues/priorityqueue"
)

// PriorityQueue is a priority queue implementation.
type PriorityQueue[T comparable] struct {
	queue *pq.Queue
}

// NewPriorityQueue creates a new PriorityQueue using a comparator.
func NewPriorityQueue[T comparable](comparator func(a, b T) int) PriorityQueue[T] {
	queue := pq.NewWith(func(a, b any) int {
		return comparator(a.(T), b.(T))
	})
	return PriorityQueue[T]{
		queue: queue,
	}
}

// Push pushes a new item.
func (p PriorityQueue[T]) Push(t T) {
	p.queue.Enqueue(t)
}

// Pop pops a new item.
func (p PriorityQueue[T]) Pop() (T, bool) {
	v, ok := p.queue.Dequeue()
	if ok {
		return v.(T), ok
	}
	var zero T
	return zero, false
}

// Peek peeks a new item (do not remove from the queue).
func (p PriorityQueue[T]) Peek() (T, bool) {
	v, ok := p.queue.Peek()
	if ok {
		return v.(T), ok
	}
	var zero T
	return zero, false
}

// IsEmpty checks if the priority queue is empty.
func (p PriorityQueue[T]) IsEmpty() bool {
	return p.queue.Empty()
}

// Size returns the priority queue size.
func (p PriorityQueue[T]) Size() int {
	return p.queue.Size()
}

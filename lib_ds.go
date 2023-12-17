package aoc

// PriorityQueue is a priority queue implementation.
type PriorityQueue[T comparable] struct {
	items      []T
	comparator func(a, b T) int
}

// NewPriorityQueue creates a new PriorityQueue using a comparator.
func NewPriorityQueue[T comparable](comparator func(a, b T) int) PriorityQueue[T] {
	return PriorityQueue[T]{comparator: comparator}
}

// Push pushes a new item.
func (pq *PriorityQueue[T]) Push(item T) {
	pq.items = append(pq.items, item)
	pq.heapifyUp(len(pq.items) - 1)
}

// Pop removes and returns the top element from the priority queue.
func (pq *PriorityQueue[T]) Pop() (T, bool) {
	if len(pq.items) == 0 {
		var zero T
		return zero, false
	}
	top := pq.items[0]
	lastIndex := len(pq.items) - 1
	pq.items[0], pq.items[lastIndex] = pq.items[lastIndex], pq.items[0]
	pq.items = pq.items[:lastIndex]
	pq.heapifyDown(0)
	return top, true
}

// Peek returns the top element of the priority queue without removing it.
func (pq *PriorityQueue[T]) Peek() (T, bool) {
	if len(pq.items) == 0 {
		var zero T
		return zero, false
	}
	return pq.items[0], true
}

// Len returns the number of elements in the priority queue.
func (pq *PriorityQueue[T]) Len() int {
	return len(pq.items)
}

// IsEmpty checks if the priority queue is empty.
func (pq *PriorityQueue[T]) IsEmpty() bool {
	return len(pq.items) == 0
}

func (pq *PriorityQueue[T]) heapifyUp(index int) {
	for index > 0 {
		parentIndex := (index - 1) / 2
		if pq.comparator(pq.items[index], pq.items[parentIndex]) < 0 {
			pq.items[index], pq.items[parentIndex] = pq.items[parentIndex], pq.items[index]
			index = parentIndex
		} else {
			break
		}
	}
}

func (pq *PriorityQueue[T]) heapifyDown(index int) {
	for {
		leftChild := 2*index + 1
		rightChild := 2*index + 2
		smallest := index

		if leftChild < len(pq.items) && pq.comparator(pq.items[leftChild], pq.items[smallest]) < 0 {
			smallest = leftChild
		}

		if rightChild < len(pq.items) && pq.comparator(pq.items[rightChild], pq.items[smallest]) < 0 {
			smallest = rightChild
		}

		if smallest != index {
			pq.items[index], pq.items[smallest] = pq.items[smallest], pq.items[index]
			index = smallest
		} else {
			break
		}
	}
}

package main

import (
	"fmt"
	"strconv"
)

func fs1(rounds int) string {
	a := Recipe{value: 3}
	b := Recipe{value: 7}
	a.next = &b
	a.previous = &b
	b.next = &a
	b.previous = &a
	head := &a
	tail := &b

	e1 := &a
	e2 := &b
	s := ""
	total := 2
	for i := 0; ; i++ {
		newRecipe := e1.value + e2.value
		if newRecipe < 10 {
			x := &Recipe{value: newRecipe, previous: tail, next: tail.next}
			tail.next = x
			tail = x
			head.previous = tail

			total++
			s = add(total, rounds, s, x)
			if len(s) >= 10 {
				return s
			}
		} else {
			x := &Recipe{value: newRecipe / 10, previous: tail}
			y := &Recipe{value: newRecipe % 10, previous: x, next: tail.next}
			tail.next = x
			x.next = y
			tail = y
			head.previous = tail

			total++
			s = add(total, rounds, s, x)
			if len(s) >= 10 {
				return s
			}

			total++
			s = add(total, rounds, s, y)
			if len(s) >= 10 {
				return s
			}
		}

		sum := e1.value + 1
		for moves := 0; moves < sum; moves++ {
			e1 = e1.next
		}
		sum = e2.value + 1
		for moves := 0; moves < sum; moves++ {
			e2 = e2.next
		}
	}
}

func add(total int, rounds int, s string, r *Recipe) string {
	if total <= rounds {
		return s
	}

	s += strconv.Itoa(r.value)
	return s
}

type Recipe struct {
	value    int
	previous *Recipe
	next     *Recipe
}

func (r *Recipe) last(count int) string {
	cur := r
	for i := 0; i < count-1; i++ {
		cur = cur.previous
	}
	s := ""
	for i := 0; i < count; i++ {
		s += strconv.Itoa(cur.value)
		cur = cur.next
	}
	return s
}

func (r *Recipe) print() {
	s := strconv.Itoa(r.value)
	cur := r.next
	for cur != r {
		s += ", " + strconv.Itoa(cur.value)
		cur = cur.next
	}
	fmt.Println(s)
}

func fs2(s string, last int) int {
	a := Recipe{value: 3}
	b := Recipe{value: 7}
	a.next = &b
	a.previous = &b
	b.next = &a
	b.previous = &a
	head := &a
	tail := &b

	e1 := &a
	e2 := &b
	total := 0
	for i := 0; ; i++ {
		newRecipe := e1.value + e2.value
		if newRecipe < 10 {
			x := &Recipe{value: newRecipe, previous: tail, next: tail.next}
			tail.next = x
			tail = x
			head.previous = tail
			total++
			if tail.last(last) == s {
				return total - last + 2
			}
		} else {
			x := &Recipe{value: newRecipe / 10, previous: tail, next: tail.next}
			tail.next = x
			tail = x
			head.previous = tail
			total++
			if tail.last(last) == s {
				return total - last + 2
			}

			y := &Recipe{value: newRecipe % 10, previous: tail, next: tail.next}
			tail.next = y
			tail = y
			head.previous = tail
			total++
			if tail.last(last) == s {
				return total - last + 2
			}
		}

		sum := e1.value + 1
		for moves := 0; moves < sum; moves++ {
			e1 = e1.next
		}
		sum = e2.value + 1
		for moves := 0; moves < sum; moves++ {
			e2 = e2.next
		}
	}
}

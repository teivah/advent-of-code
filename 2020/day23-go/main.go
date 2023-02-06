package main

import (
	"strconv"

	aoc "github.com/teivah/advent-of-code"
)

func fs1(input string, moves int) string {
	ring := toRing(input)
	for i := 0; i < moves; i++ {
		ring.round()
	}
	return ring.String()
}

type Ring struct {
	len     int
	current *Node
	nodes   map[int]*Node
}

func (r *Ring) round() {
	saved := r.current.next
	removed := make(map[int]bool)
	tmp := saved
	for i := 0; i < 3; i++ {
		removed[tmp.value] = true
		tmp = tmp.next
	}

	r.current.next = r.current.next.next.next.next
	r.current.next.previous = r.current

	destinationValue := r.current.value - 1
	if destinationValue == 0 {
		destinationValue = r.len
	}
	for removed[destinationValue] {
		destinationValue--
		if destinationValue == 0 {
			destinationValue = r.len
		}
	}

	destination := r.nodes[destinationValue]

	next := destination.next
	destination.next = saved
	saved.previous = destination
	saved.next.next.next = next
	next.previous = saved.next.next

	r.current = r.current.next
}

func (r *Ring) String() string {
	s := ""
	cur := r.nodes[1].next
	for i := 0; i < r.len-1; i++ {
		s += strconv.Itoa(cur.value)
		cur = cur.next
	}
	return s
}

func toRing(s string) *Ring {
	var values []int
	for i := 0; i < len(s); i++ {
		r := rune(s[i])
		v := aoc.RuneToInt(r)
		values = append(values, v)
	}

	nodes := make(map[int]*Node)
	head := &Node{value: values[0]}
	nodes[values[0]] = head
	previous := head
	for i := 1; i < len(values); i++ {
		cur := &Node{
			value:    values[i],
			previous: previous,
		}
		nodes[values[i]] = cur
		previous.next = cur
		previous = cur
	}
	previous.next = head
	head.previous = previous
	return &Ring{
		len:     len(values),
		current: head,
		nodes:   nodes,
	}
}

type Node struct {
	value    int
	previous *Node
	next     *Node
}

func (n *Node) String() string {
	return strconv.Itoa(n.value)
}

func fs2(input string, moves int) string {
	ring := toRing2(input)
	for i := 0; i < moves; i++ {
		ring.round()
	}
	one := ring.nodes[1]
	res := one.next.value * one.next.next.value
	return strconv.Itoa(res)
}

func toRing2(s string) *Ring {
	var values []int
	for i := 0; i < len(s); i++ {
		r := rune(s[i])
		v := aoc.RuneToInt(r)
		values = append(values, v)
	}

	nodes := make(map[int]*Node)
	maxer := aoc.NewMaxer()
	maxer.Add(values[0])
	head := &Node{value: values[0]}
	nodes[values[0]] = head
	previous := head
	for i := 1; i < len(values); i++ {
		maxer.Add(values[i])
		cur := &Node{
			value:    values[i],
			previous: previous,
		}
		nodes[values[i]] = cur
		previous.next = cur
		previous = cur
	}
	from := maxer.Get() + 1
	for i := from; i <= 1_000_000; i++ {
		cur := &Node{
			value:    i,
			previous: previous,
		}
		nodes[i] = cur
		previous.next = cur
		previous = cur
	}

	previous.next = head
	head.previous = previous
	return &Ring{
		len:     1_000_000,
		current: head,
		nodes:   nodes,
	}
}

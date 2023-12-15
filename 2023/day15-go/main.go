package main

import (
	"io"
	"strings"

	aoc "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	del := aoc.NewDelimiter(aoc.ReaderToString(input), ",")
	res := 0
	for _, s := range del.GetStrings() {
		res += hash(s)
	}
	return res
}

func hash(s string) int {
	res := 0
	for _, c := range s {
		res += int(c)
		res *= 17
		res %= 256
	}
	return res
}

type Box struct {
	tail   *Lense
	labels map[string]*Lense
}

type Lense struct {
	label    string
	size     int
	next     *Lense
	previous *Lense
}

func fs2(input io.Reader) int {
	del := aoc.NewDelimiter(aoc.ReaderToString(input), ",")
	boxes := make([]*Box, 256)
	for i := 0; i < 256; i++ {
		boxes[i] = &Box{
			labels: make(map[string]*Lense),
		}
	}
	for _, s := range del.GetStrings() {
		handleOperation(boxes, s)
	}

	res := 0
	for idx, box := range boxes {
		res += getPower(idx+1, box)
	}

	return res
}

func handleOperation(boxes []*Box, s string) {
	if strings.Contains(s, "-") {
		label := s[:len(s)-1]
		idx := hash(label)
		box := boxes[idx]

		if lense, exists := box.labels[label]; exists {
			delete(box.labels, label)
			tail := box.tail
			if tail.label == lense.label {
				if tail.next != nil {
					box.tail = tail.next
					box.tail.previous = nil
				} else {
					box.tail = nil
				}
				return
			}

			next := lense.next
			if lense.previous != nil {
				lense.previous.next = next
				if next != nil {
					next.previous = lense.previous
				}
			}
		}
	} else {
		del := aoc.NewDelimiter(s, "=")
		label := del.GetString(0)
		idx := hash(label)
		size := del.GetInt(1)
		box := boxes[idx]

		if lense, exists := box.labels[label]; exists {
			lense.size = size
		} else {
			tail := box.tail
			l := &Lense{
				label: label,
				size:  size,
			}
			if tail == nil {
				box.tail = l
			} else {
				tail.previous = l
				l.next = tail
				box.tail = l
			}
			box.labels[label] = l
		}
	}
}

func getPower(idx int, box *Box) int {
	// Finding head
	cur := box.tail
	if cur == nil {
		return 0
	}
	for {
		if cur.next == nil {
			break
		}
		cur = cur.next
	}

	res := 0
	i := 1
	for cur != nil {
		res += cur.size * i * idx
		i++
		cur = cur.previous
	}
	return res
}

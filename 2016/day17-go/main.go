package main

import (
	"crypto/md5"
	"encoding/hex"
	"fmt"
)

func fs1(passcode string) string {
	var q []Pos
	q = append(q, Pos{0, 0, ""})
	for len(q) != 0 {
		pos := q[0]
		q = q[1:]

		if pos.row == vault && pos.col == vault {
			return pos.path
		}
		if pos.row < 0 || pos.row > vault || pos.col < 0 || pos.col > vault {
			continue
		}

		d := doors(passcode, pos.path)
		if d.up {
			q = append(q, Pos{pos.row - 1, pos.col, pos.path + "U"})
		}
		if d.down {
			q = append(q, Pos{pos.row + 1, pos.col, pos.path + "D"})
		}
		if d.left {
			q = append(q, Pos{pos.row, pos.col - 1, pos.path + "L"})
		}
		if d.right {
			q = append(q, Pos{pos.row, pos.col + 1, pos.path + "R"})
		}
	}

	return ""
}

var vault = 3

func hash(text string) string {
	h := md5.Sum([]byte(text))
	return hex.EncodeToString(h[:])
}

type Pos struct {
	row  int
	col  int
	path string
}

type Doors struct {
	up    bool
	down  bool
	left  bool
	right bool
}

func doors(passcode string, path string) Doors {
	h := hash(passcode + path)
	return Doors{
		up:    isOpened(h[0]),
		down:  isOpened(h[1]),
		left:  isOpened(h[2]),
		right: isOpened(h[3]),
	}
}

func isOpened(r uint8) bool {
	if r == 'b' || r == 'c' || r == 'd' || r == 'e' || r == 'f' {
		return true
	}
	return false
}

type Queue struct {
	head *Node
	tail *Node
}

type Node struct {
	pos      Pos
	previous *Node
	next     *Node
}

func NewQueue() *Queue {
	return &Queue{}
}

func (q *Queue) print() {
	cur := q.head
	for cur != nil {
		fmt.Printf("%v \n", cur.pos)
		cur = cur.next
	}
}

func (q *Queue) push(name Pos) {
	node := &Node{pos: name}

	if q.tail == nil {
		q.head = node
		q.tail = node
		return
	}

	node.previous = q.tail
	q.tail.next = node
	q.tail = node
}

func (q *Queue) isEmpty() bool {
	return q.head == nil
}

func (q *Queue) peekHead() Pos {
	return q.head.pos
}

func (q *Queue) popHead() Pos {
	n := q.head.pos

	if q.head == q.tail {
		q.head = nil
		q.tail = nil
	} else if q.head.next == q.tail {
		q.head = q.tail
		q.tail.previous = nil
	} else {
		q.head = q.head.next
	}

	return n
}

func (q *Queue) pop() Pos {
	n := q.tail.pos

	if q.head == q.tail {
		q.head = nil
		q.tail = nil
	} else if q.head.next == q.tail {
		q.tail = q.head
	} else {
		q.tail = q.tail.previous
	}

	return n
}

type Entry struct {
	row   int
	col   int
	up    bool
	down  bool
	left  bool
	right bool
}

func fs2(passcode string) int {
	cache := make(map[Entry]struct{})

	var q []Pos
	q = append(q, Pos{0, 0, ""})
	max := 0
	for len(q) != 0 {
		pos := q[0]
		q = q[1:]

		if pos.row == vault && pos.col == vault {
			if len(pos.path) > max {
				max = len(pos.path)
			}
			continue
		}
		if pos.row < 0 || pos.row > vault || pos.col < 0 || pos.col > vault {
			continue
		}

		d := doors(passcode, pos.path)

		entry := Entry{
			row:   pos.row,
			col:   pos.col,
			up:    d.up,
			down:  d.down,
			left:  d.left,
			right: d.right,
		}
		if _, exists := cache[entry]; exists {
			continue
		}
		//cache[entry] = struct{}{}

		if d.up {
			q = append(q, Pos{pos.row - 1, pos.col, pos.path + "U"})
		}
		if d.down {
			q = append(q, Pos{pos.row + 1, pos.col, pos.path + "D"})
		}
		if d.left {
			q = append(q, Pos{pos.row, pos.col - 1, pos.path + "L"})
		}
		if d.right {
			q = append(q, Pos{pos.row, pos.col + 1, pos.path + "R"})
		}
	}

	return max
}

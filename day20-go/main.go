package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"strconv"
)

type Ring struct {
	len  int
	head *Node
	pos  map[int]*Node
}

func NewRing(s []int) *Ring {
	previous := &Node{
		value: s[0],
	}
	pos := make(map[int]*Node, len(s))
	pos[0] = previous
	head := previous
	for i := 1; i < len(s); i++ {
		v := s[i]
		current := &Node{
			previous: previous,
			value:    v,
		}
		pos[i] = current
		previous.next = current
		previous = current
	}
	previous.next = head
	head.previous = previous
	return &Ring{
		len:  len(s),
		head: head,
		pos:  pos,
	}
}

func (r *Ring) move(i int) {
	nodeA := r.pos[i]
	to := nodeA.value
	if to == 0 {
		return
	}

	if to > r.len {
		to = to % (r.len - 1)
	}

	if to < -r.len {
		to = to % (r.len - 1)
	}

	nodeB := nodeA
	if to > 0 {
		for i := 0; i < to; i++ {
			nodeB = nodeB.next
			if nodeB == nodeA {
				i--
			}
		}
	} else {
		for i := to; i <= 0; i++ {
			nodeB = nodeB.previous
			if nodeB == nodeA {
				i--
			}
		}
	}

	if nodeB.next == nodeA {
		return
	}

	tmpPreviouA := nodeA.previous
	tmpNextA := nodeA.next
	tmpNextB := nodeB.next

	if nodeA == r.head {
		r.head = tmpNextA
	} else if nodeB.next == r.head {
		r.head = nodeA
	}

	nodeA.previous.next = nodeA.next
	nodeA.next.previous = tmpPreviouA

	nodeB.next = nodeA
	nodeA.previous = nodeB

	nodeA.next = tmpNextB
	tmpNextB.previous = nodeA
}

func (r *Ring) getData() []int {
	res := make([]int, 0, r.len)
	cur := r.head
	res = append(res, cur.value)
	cur = cur.next
	for cur != r.head {
		res = append(res, cur.value)
		cur = cur.next
	}
	return res
}

func (r *Ring) print() {
	cur := r.head
	fmt.Printf("%v ", cur.value)
	cur = cur.next
	for cur != r.head {
		fmt.Printf("%v ", cur.value)
		cur = cur.next
	}
	fmt.Println()
}

func (r *Ring) write() {
	f, err := os.CreateTemp("/tmp", "v")
	if err != nil {
		panic(err)
	}
	defer f.Close()

	cur := r.head
	f.WriteString(fmt.Sprintf("%d\n", cur.value))
	cur = cur.next
	i := 0
	for cur != r.head {
		f.WriteString(fmt.Sprintf("%d\n", cur.value))
		cur = cur.next
		fmt.Println(i)
	}
	fmt.Println(f.Name())
}

func (r *Ring) get(s []int) []int {
	res := make([]int, len(s))
	for i, v := range s {
		res[i] = r.getI(v)
	}
	return res
}

func (r *Ring) getI(v int) int {
	cur := r.head
	for {
		if cur.value == 0 {
			break
		}
		cur = cur.next
	}
	for i := 0; i < v; i++ {
		cur = cur.next
	}
	return cur.value
}

type Node struct {
	previous *Node
	next     *Node
	value    int
}

func fs1(input io.Reader, res []int) (int, error) {
	scanner := bufio.NewScanner(input)
	var data []int
	for scanner.Scan() {
		line := scanner.Text()
		i, err := strconv.Atoi(line)
		if err != nil {
			return 0, err
		}
		data = append(data, i)
	}

	r := NewRing(data)

	for i := 0; i < r.len; i++ {
		r.move(i)
	}

	get := r.get(res)
	fmt.Println(get)
	sum := 0
	for _, v := range get {
		sum += v
	}

	return sum, nil
}

func fs2(input io.Reader, res []int, decryptionKey int, mix int) (int, error) {
	scanner := bufio.NewScanner(input)
	var data []int
	for scanner.Scan() {
		line := scanner.Text()
		i, err := strconv.Atoi(line)
		if err != nil {
			return 0, err
		}
		data = append(data, i*decryptionKey)
	}

	r := NewRing(data)

	for j := 0; j < mix; j++ {
		for i := 0; i < r.len; i++ {
			r.move(i)
		}
		fmt.Println(j)
	}

	get := r.get(res)
	fmt.Println(get)
	sum := 0
	for _, v := range get {
		sum += v
	}

	return sum, nil
}

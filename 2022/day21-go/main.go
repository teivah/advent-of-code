package main

import (
	"bufio"
	"fmt"
	"io"
	"strconv"
)

type Operator int

const (
	Plus Operator = iota
	Minus
	Divide
	Multiply
	NumberOnly
)

type Monkey struct {
	name     string
	operator Operator
	monkey1  string
	monkey2  string
	number   int
}

func NewMonkey(s string) (Monkey, error) {
	name := s[:4]
	c := s[6]
	if c >= 'a' && c <= 'z' {
		monkey1 := s[6:10]
		monkey2 := s[13:]
		var operator Operator
		switch s[11] {
		case '+':
			operator = Plus
		case '-':
			operator = Minus
		case '*':
			operator = Multiply
		case '/':
			operator = Divide
		}
		return Monkey{
			name:     name,
			operator: operator,
			monkey1:  monkey1,
			monkey2:  monkey2,
		}, nil
	}
	number, err := strconv.Atoi(s[6:])
	if err != nil {
		return Monkey{}, err
	}
	return Monkey{
		name:     name,
		operator: NumberOnly,
		number:   number,
	}, nil
}

func (m Monkey) dependencies() []string {
	if m.operator == NumberOnly {
		return nil
	}

	return []string{m.monkey1, m.monkey2}
}

func fs1(input io.Reader) (int, error) {
	scanner := bufio.NewScanner(input)
	monkeys := make(map[string]Monkey)
	for scanner.Scan() {
		line := scanner.Text()
		monkey, err := NewMonkey(line)
		if err != nil {
			return 0, err
		}
		monkeys[monkey.name] = monkey
	}

	sort := topologicalSort(monkeys)
	for i := len(sort) - 1; i >= 0; i-- {
		monkey := monkeys[sort[i]]
		if monkey.operator == NumberOnly {
			continue
		}

		number := 0
		switch monkey.operator {
		case Plus:
			number = monkeys[monkey.monkey1].number + monkeys[monkey.monkey2].number
		case Minus:
			number = monkeys[monkey.monkey1].number - monkeys[monkey.monkey2].number
		case Divide:
			number = monkeys[monkey.monkey1].number / monkeys[monkey.monkey2].number
		case Multiply:
			number = monkeys[monkey.monkey1].number * monkeys[monkey.monkey2].number
		}

		monkey.number = number
		monkeys[sort[i]] = monkey
	}

	return monkeys["root"].number, nil
}

func topologicalSort(monkeys map[string]Monkey) []string {
	inDegreeCounter := make(map[string]int, len(monkeys))
	for monkey := range monkeys {
		inDegreeCounter[monkey] = 0
	}

	for _, monkey := range monkeys {
		deps := monkey.dependencies()
		if len(deps) == 0 {
			continue
		}
		for _, dep := range deps {
			inDegreeCounter[dep]++
		}
	}

	q := NewQueue()
	for k, v := range inDegreeCounter {
		if v == 0 {
			q.push(k)
		}
	}

	res := make([]string, 0, len(monkeys))
	for !q.isEmpty() {
		name := q.pop()
		res = append(res, name)
		monkey := monkeys[name]
		inDegreeCounter[monkey.monkey1]--
		if inDegreeCounter[monkey.monkey1] == 0 {
			q.push(monkey.monkey1)
		}
		inDegreeCounter[monkey.monkey2]--
		if inDegreeCounter[monkey.monkey2] == 0 {
			q.push(monkey.monkey2)
		}
	}

	return res
}

type Queue struct {
	head *Node
	tail *Node
}

type Node struct {
	name     string
	previous *Node
	next     *Node
}

func NewQueue() *Queue {
	return &Queue{}
}

func (q *Queue) print() {
	cur := q.head
	for cur != nil {
		fmt.Printf("%v \n", cur.name)
		cur = cur.next
	}
}

func (q *Queue) push(name string) {
	node := &Node{name: name}

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

func (q *Queue) peekHead() string {
	return q.head.name
}

func (q *Queue) popHead() string {
	n := q.head.name

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

func (q *Queue) pop() string {
	n := q.tail.name

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

func fs2(input io.Reader) (int, error) {
	scanner := bufio.NewScanner(input)
	monkeys := make(map[string]Monkey)
	for scanner.Scan() {
		line := scanner.Text()
		monkey, err := NewMonkey(line)
		if err != nil {
			return 0, err
		}
		monkeys[monkey.name] = monkey
	}

	sort := topologicalSort(monkeys)
	for i := len(sort) - 1; i >= 0; i-- {
		monkey := monkeys[sort[i]]
		if monkey.operator == NumberOnly {
			continue
		}

		number := 0
		switch monkey.operator {
		case Plus:
			number = monkeys[monkey.monkey1].number + monkeys[monkey.monkey2].number
		case Minus:
			number = monkeys[monkey.monkey1].number - monkeys[monkey.monkey2].number
		case Divide:
			number = monkeys[monkey.monkey1].number / monkeys[monkey.monkey2].number
		case Multiply:
			number = monkeys[monkey.monkey1].number * monkeys[monkey.monkey2].number
		}

		monkey.number = number
		monkeys[sort[i]] = monkey
	}

	pathToHuman := NewQueue()
	isBranch(monkeys, "root", pathToHuman)

	root := monkeys["root"]
	isPath1 := pathToHuman.peekHead() == root.monkey1
	target := 0
	if isPath1 {
		target = monkeys[root.monkey2].number
	} else {
		target = monkeys[root.monkey1].number
	}

	return getResult(monkeys, pathToHuman.popHead(), pathToHuman, target), nil
}

// This node has to return target
func getResult(monkeys map[string]Monkey, current string, pathToHuman *Queue, target int) int {
	if current == "humn" {
		fmt.Println(target)
		return target
	}

	monkey := monkeys[current]
	head := pathToHuman.peekHead()
	isPath1 := head == monkey.monkey1
	other := 0
	if isPath1 {
		other = monkeys[monkey.monkey2].number
	} else {
		other = monkeys[monkey.monkey1].number
	}

	newTarget := 0
	switch monkey.operator {
	case Plus:
		newTarget = target - other
	case Minus:
		if isPath1 {
			newTarget = target + other
		} else {
			newTarget = other - target
		}
	case Divide:
		if isPath1 {
			newTarget = target * other
		} else {
			newTarget = other / target
		}
	case Multiply:
		newTarget = target / other
	}

	c := ""
	switch monkey.operator {
	case Plus:
		c = "+"
	case Minus:
		c = "-"
	case Divide:
		c = "/"
	case Multiply:
		c = "*"
	}
	fmt.Printf("target=%v, other=%v, sign=%#v\n", target, other, c)

	pop := pathToHuman.popHead()
	return getResult(monkeys, pop, pathToHuman, newTarget)
}

func isBranch(monkeys map[string]Monkey, name string, q *Queue) bool {
	monkey := monkeys[name]

	if monkey.name == "humn" {
		return true
	}

	if monkey.operator == NumberOnly {
		return false
	}

	q.push(monkey.monkey1)
	if isBranch(monkeys, monkey.monkey1, q) {
		return true
	}

	q.pop()
	q.push(monkey.monkey2)
	if isBranch(monkeys, monkey.monkey2, q) {
		return true
	}

	q.pop()
	return false
}

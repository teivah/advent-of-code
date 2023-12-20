package main

import (
	"fmt"
	"io"

	aoc "github.com/teivah/advent-of-code"
)

type pulseType int

func (p pulseType) String() string {
	if p == lowPulse {
		return "low"
	}
	return "high"
}

const (
	lowPulse pulseType = iota
	highPulse
)

var pulsesSent map[pulseType]int
var res2 int
var rxParent string

var debug = false

type module interface {
	pulseAction(it int, id string, p pulseType) []Pulse
}

type Pulse struct {
	from        string
	destination module
	p           pulseType
}

type broadcaster struct {
	id           string
	destinations []string
	modules      map[string]module
}

func (b *broadcaster) pulseAction(it int, _ string, p pulseType) []Pulse {
	pulses := make([]Pulse, 0, len(b.destinations))
	for _, destination := range b.destinations {
		destinationModule, contains := b.modules[destination]
		if !contains {
			pulsesSent[p]++
			if debug {
				fmt.Printf("%s -%s-> %s\n", b.id, p, destination)
			}
			continue
		}

		pulses = append(pulses, Pulse{
			destination: destinationModule,
			from:        b.id,
			p:           p,
		})
		pulsesSent[p]++
		if debug {
			fmt.Printf("%s -%s-> %s\n", b.id, p, destination)
		}
	}
	return pulses
}

type flipFlop struct {
	id           string
	destinations []string
	modules      map[string]module
	on           bool
}

func (f *flipFlop) pulseAction(it int, _ string, p pulseType) []Pulse {
	if p == highPulse {
		return nil
	}

	output := lowPulse
	if !f.on {
		output = highPulse
	}
	f.on = !f.on

	pulses := make([]Pulse, 0, len(f.destinations))
	for _, destination := range f.destinations {
		destinationModule, contains := f.modules[destination]
		if !contains {
			pulsesSent[output]++
			if debug {
				fmt.Printf("%s -%s-> %s\n", f.id, output, destination)
			}
			continue
		}

		pulses = append(pulses, Pulse{
			destination: destinationModule,
			from:        f.id,
			p:           output,
		})
		pulsesSent[output]++
		if debug {
			fmt.Printf("%s -%s-> %s\n", f.id, output, destination)
		}
	}
	return pulses
}

type conjunction struct {
	id           string
	inputs       map[string]pulseType
	destinations []string
	modules      map[string]module
	latest       map[string]int
	multiples    map[string]int
}

func (c *conjunction) setInputs(inputs []string) {
	c.inputs = make(map[string]pulseType)
	for _, input := range inputs {
		c.inputs[input] = lowPulse
	}
}

func (c *conjunction) pulseAction(it int, id string, p pulseType) []Pulse {
	c.inputs[id] = p
	if c.id == rxParent {
		if c.inputs[id] == highPulse {
			v, contains := c.latest[id]
			if !contains {
				c.latest[id] = it
			} else {
				if v != it {
					c.multiples[id] = it - v

					if len(c.multiples) == len(c.latest) {
						var numbers []int
						for _, v := range c.multiples {
							numbers = append(numbers, v)
						}
						res2 = aoc.LeastCommonMultiple(numbers)
						return nil
					}
					c.latest[id] = it
				}
			}
		}
	}

	output := lowPulse
	for _, input := range c.inputs {
		if input == lowPulse {
			output = highPulse
			break
		}
	}

	pulses := make([]Pulse, 0, len(c.destinations))
	for _, destination := range c.destinations {
		destinationModule, contains := c.modules[destination]
		if !contains {
			pulsesSent[output]++
			if debug {
				fmt.Printf("%s -%s-> %s\n", c.id, output, destination)
			}
			continue
		}

		pulses = append(pulses, Pulse{
			destination: destinationModule,
			from:        c.id,
			p:           output,
		})
		pulsesSent[output]++
		if debug {
			fmt.Printf("%s -%s-> %s\n", c.id, output, destination)
		}
	}
	return pulses
}

func fs1(input io.Reader, iterations int) int {
	modules, _ := parse(input)

	for i := 0; i < iterations; i++ {
		if debug {
			fmt.Printf("iteration %d\n", i+1)
			fmt.Println("button -low-> broadcaster")
		}
		actions := modules["broadcaster"].pulseAction(i, "broadcaster", lowPulse)
		pulsesSent[lowPulse]++

		q := actions
		for len(q) != 0 {
			pulse := q[0]
			q = q[1:]

			actions = pulse.destination.pulseAction(i, pulse.from, pulse.p)
			q = append(q, actions...)
		}
	}

	if debug {
		fmt.Println("low ", pulsesSent[lowPulse], " high ", pulsesSent[highPulse])
	}

	return pulsesSent[lowPulse] * pulsesSent[highPulse]
}

func parse(input io.Reader) (map[string]module, map[string][]string) {
	pulsesSent = make(map[pulseType]int)

	lines := aoc.ReaderToStrings(input)
	graph := make(map[string][]string)
	modules := make(map[string]module)
	conjuctions := make(map[string]*conjunction)
	inDegree := make(map[string][]string)

	for _, line := range lines {
		del := aoc.NewDelimiter(line, "->", aoc.WithTrimSpace())

		name := del.GetString(0)
		destinationsStr := del.GetString(1)
		del2 := aoc.NewDelimiter(destinationsStr, ", ")
		destinations := del2.GetStrings()

		if name == "broadcaster" {
			p := &broadcaster{
				id:           name,
				destinations: destinations,
				modules:      modules,
			}
			modules[name] = p
		} else if name[0] == '%' {
			name = name[1:]
			p := &flipFlop{
				id:           name,
				destinations: destinations,
				modules:      modules,
			}
			modules[name] = p
		} else if name[0] == '&' {
			name = name[1:]
			p := &conjunction{
				id:           name,
				inputs:       nil,
				destinations: destinations,
				modules:      modules,
				latest:       make(map[string]int),
				multiples:    make(map[string]int),
			}
			conjuctions[name] = p
			modules[name] = p
		} else {
			panic(name)
		}

		graph[name] = destinations
		for _, destination := range destinations {
			inDegree[destination] = append(inDegree[destination], name)
		}
	}

	for name, c := range conjuctions {
		c.setInputs(inDegree[name])
	}

	return modules, graph
}

func fs2(input io.Reader) int {
	modules, graph := parse(input)

	parent := dependency(graph, "broadcaster", nil, make(map[string]*Node))
	target := find(parent, "rx")
	for k := range target.parents {
		rxParent = k
		break
	}

	for i := 0; ; i++ {
		if debug {
			fmt.Printf("iteration %d\n", i+1)
			fmt.Println("button -low-> broadcaster")
		}
		actions := modules["broadcaster"].pulseAction(i, "broadcaster", lowPulse)
		pulsesSent[lowPulse]++

		q := actions
		for len(q) != 0 {
			pulse := q[0]
			q = q[1:]

			actions = pulse.destination.pulseAction(i, pulse.from, pulse.p)
			q = append(q, actions...)
		}

		if res2 != 0 {
			return res2
		}
	}
}

type Node struct {
	id       string
	parents  map[string]*Node
	children []*Node
}

func dependency(graph map[string][]string, id string, currentParent *Node, nodes map[string]*Node) *Node {
	if n, exists := nodes[id]; exists {
		if currentParent != nil {
			n.parents[currentParent.id] = currentParent
		}
		return n
	}

	parents := make(map[string]*Node)
	if currentParent != nil {
		parents[currentParent.id] = currentParent
	}

	node := &Node{
		id:      id,
		parents: parents,
	}
	nodes[id] = node
	destinations, exists := graph[id]
	if !exists {
		return node
	}

	var children []*Node
	for _, destination := range destinations {
		children = append(children, dependency(graph, destination, node, nodes))
	}
	node.children = children
	return node
}

func find(node *Node, target string) *Node {
	if node.id == target {
		return node
	}

	for _, child := range node.children {
		if n := find(child, target); n != nil {
			return n
		}
	}
	return nil
}

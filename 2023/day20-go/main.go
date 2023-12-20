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

var debug = true

var rx = false

type module interface {
	pulseAction(id string, p pulseType) []Pulse
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

func (b *broadcaster) pulseAction(_ string, p pulseType) []Pulse {
	pulses := make([]Pulse, 0, len(b.destinations))
	for _, destination := range b.destinations {
		if destination == "rx" && p == lowPulse {
			rx = true
		}

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

func (f *flipFlop) pulseAction(_ string, p pulseType) []Pulse {
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
		if destination == "rx" && output == lowPulse {
			rx = true
		}

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
}

func (c *conjunction) setInputs(inputs []string) {
	c.inputs = make(map[string]pulseType)
	for _, input := range inputs {
		c.inputs[input] = lowPulse
	}
}

func (c *conjunction) pulseAction(id string, p pulseType) []Pulse {
	c.inputs[id] = p
	output := lowPulse
	for _, input := range c.inputs {
		if input == lowPulse {
			output = highPulse
			break
		}
	}

	pulses := make([]Pulse, 0, len(c.destinations))
	for _, destination := range c.destinations {
		if destination == "rx" && output == lowPulse {
			rx = true
		}

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
	modules := parse(input)

	for i := 0; i < iterations; i++ {
		if debug {
			fmt.Printf("iteration %d\n", i+1)
			fmt.Println("button -low-> broadcaster")
		}
		actions := modules["broadcaster"].pulseAction("broadcaster", lowPulse)
		pulsesSent[lowPulse]++

		q := actions
		for len(q) != 0 {
			pulse := q[0]
			q = q[1:]

			actions = pulse.destination.pulseAction(pulse.from, pulse.p)
			q = append(q, actions...)
		}
	}

	if debug {
		fmt.Println("low ", pulsesSent[lowPulse], " high ", pulsesSent[highPulse])
	}

	return pulsesSent[lowPulse] * pulsesSent[highPulse]
}

func parse(input io.Reader) map[string]module {
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

	return modules
}

func fs2(input io.Reader) int {
	modules := parse(input)

	for i := 0; ; i++ {
		if rx {
			return i
		}

		if debug {
			fmt.Printf("iteration %d\n", i+1)
			fmt.Println("button -low-> broadcaster")
		}
		actions := modules["broadcaster"].pulseAction("broadcaster", lowPulse)
		pulsesSent[lowPulse]++

		q := actions
		for len(q) != 0 {
			pulse := q[0]
			q = q[1:]

			actions = pulse.destination.pulseAction(pulse.from, pulse.p)
			q = append(q, actions...)
		}
	}
}

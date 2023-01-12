package main

import (
	"bufio"
	"fmt"
	"io"
	"strconv"
	"strings"
)

func fs1(input io.Reader, value1, value2 int) (int, error) {
	scanner := bufio.NewScanner(input)
	var lines []string
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}

	bots := make(map[int]*Bot)
	var full []int
	instructions := make(map[int]Instruction)
	for _, s := range lines {
		if s[0] == 'v' {
			idx := indexAll(s, " ")
			value := toint(s[idx[0]+1 : idx[1]])
			id := toint(s[idx[4]+1:])

			bot, exists := bots[id]
			if !exists {
				bot = &Bot{}
				bots[id] = bot
			}
			if bot.Put(value) == 2 {
				full = append(full, id)
			}
			bots[id] = bot
		} else {
			id, instruction := ins(s)
			instructions[id] = instruction
		}
	}

	for len(full) != 0 {
		id := full[0]
		full = full[1:]

		bot := bots[id]
		low, high := bot.Get()
		if low == value1 && high == value2 {
			return id, nil
		}

		ins := instructions[id]
		if ins.lowBot != nil {
			v := *ins.lowBot
			if _, exists := bots[v]; !exists {
				bots[v] = &Bot{}
			}
			if bots[v].Put(low) == 2 {
				full = append(full, v)
			}
		}
		if ins.highBot != nil {
			v := *ins.highBot
			if _, exists := bots[v]; !exists {
				bots[v] = &Bot{}
			}
			if bots[v].Put(high) == 2 {
				full = append(full, v)
			}
		}
	}

	return -1, nil
}

func print(id int, ins Instruction) {
	s := strconv.Itoa(id)
	s += ": low to "
	if ins.lowOutput != nil {
		s += fmt.Sprintf("output %d", *ins.lowOutput)
	} else {
		s += fmt.Sprintf("robot %d", *ins.lowBot)
	}
	s += " and high to "
	if ins.highOutput != nil {
		s += fmt.Sprintf("output %d", *ins.highOutput)
	} else {
		s += fmt.Sprintf("output %d", *ins.highBot)
	}
	fmt.Println(s)
}

type Instruction struct {
	lowOutput  *int
	lowBot     *int
	highOutput *int
	highBot    *int
}

func ins(s string) (int, Instruction) {
	idx := indexAll(s, " ")
	id := toint(s[idx[0]+1 : idx[1]])
	ins := Instruction{}

	low := toint(s[idx[5]+1 : idx[6]])
	high := toint(s[idx[10]+1:])
	if strings.Index(s, "low to output") != -1 {
		ins.lowOutput = &low
	} else {
		ins.lowBot = &low
	}
	if strings.Index(s, "and high to output") != -1 {
		ins.highOutput = &high
	} else {
		ins.highBot = &high
	}

	return id, ins
}

func toint(s string) int {
	i, err := strconv.Atoi(s)
	if err != nil {
		panic(err)
	}
	return i
}

func indexAll(s string, search string) []int {
	i := 0
	var res []int
	for i < len(s) {
		index := strings.Index(s[i:], search)
		if index == -1 {
			return res
		}
		res = append(res, index+i)
		i += index + len(search)
	}
	return res
}

type Bot struct {
	a *int
	b *int
}

func (b *Bot) Get() (int, int) {
	v1 := *b.a
	v2 := *b.b
	b.a = nil
	b.b = nil
	return v1, v2
}

func (b *Bot) Put(v int) int {
	if b.a == nil {
		b.a = &v
		return 1
	} else if b.b == nil {
		if *b.a < v {
			b.b = &v
		} else {
			b.b = b.a
			b.a = &v
		}
		return 2
	} else {
		panic(b)
	}
}

func fs2(input io.Reader) (int, error) {
	scanner := bufio.NewScanner(input)
	var lines []string
	for scanner.Scan() {
		lines = append(lines, scanner.Text())
	}

	bots := make(map[int]*Bot)
	var full []int
	instructions := make(map[int]Instruction)
	for _, s := range lines {
		if s[0] == 'v' {
			idx := indexAll(s, " ")
			value := toint(s[idx[0]+1 : idx[1]])
			id := toint(s[idx[4]+1:])

			bot, exists := bots[id]
			if !exists {
				bot = &Bot{}
				bots[id] = bot
			}
			if bot.Put(value) == 2 {
				full = append(full, id)
			}
			bots[id] = bot
		} else {
			id, instruction := ins(s)
			instructions[id] = instruction
		}
	}

	outputs := make(map[int]int)
	for len(full) != 0 {
		id := full[0]
		full = full[1:]

		bot := bots[id]
		low, high := bot.Get()

		ins := instructions[id]
		if ins.lowBot != nil {
			v := *ins.lowBot
			if _, exists := bots[v]; !exists {
				bots[v] = &Bot{}
			}
			if bots[v].Put(low) == 2 {
				full = append(full, v)
			}
		} else {
			v := *ins.lowOutput
			outputs[v] = low
		}

		if ins.highBot != nil {
			v := *ins.highBot
			if _, exists := bots[v]; !exists {
				bots[v] = &Bot{}
			}
			if bots[v].Put(high) == 2 {
				full = append(full, v)
			}
		} else {
			v := *ins.highOutput
			outputs[v] = high
		}
	}

	return outputs[0] * outputs[1] * outputs[2], nil
}

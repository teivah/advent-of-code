package main

import (
	"bufio"
	"io"
	"sort"
	"time"

	lib "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	var actions []Action
	sleeps := make(map[int][]int, 0)
	for scanner.Scan() {
		line := scanner.Text()
		action := toAction(line)
		actions = append(actions, action)
		sleeps[action.id] = make([]int, 60)
	}

	sort.Slice(actions, func(i, j int) bool {
		return actions[i].t.Before(actions[j].t)
	})

	id := 0
	for i := 0; i < len(actions); i++ {
		action := actions[i]
		if action.actionType == BeginShift {
			id = action.id
			continue
		}

		start := action.t.Minute()
		end := actions[i+1].t.Minute()
		i++

		for t := start; t < end; t++ {
			sleeps[id][t]++
		}
	}

	id = -1
	mostMinutesAsleep := 0
	for k, minutes := range sleeps {
		total := 0
		for minute := 0; minute < len(minutes); minute++ {
			total += minutes[minute]
		}
		if total > mostMinutesAsleep {
			mostMinutesAsleep = total
			id = k
		}
	}

	max := 0
	minute := -1
	for i, m := range sleeps[id] {
		if m > max {
			max = m
			minute = i
		}
	}

	return id * minute
}

type Action struct {
	t          time.Time
	actionType ActionType
	id         int
}

func toAction(s string) Action {
	t, err := time.Parse("2006-01-02 15:04", s[1:17])
	if err != nil {
		panic(err)
	}

	var actionType ActionType
	id := 0
	switch s[19] {
	case 'w':
		actionType = WakesUp
	case 'f':
		actionType = FallsAsleep
	case 'G':
		actionType = BeginShift
		del := lib.NewDelimiter(s, " ")
		s := del.GetString(3)
		id = lib.StringToInt(s[1:])
	}

	return Action{
		t:          t,
		actionType: actionType,
		id:         id,
	}
}

type ActionType int

const (
	BeginShift ActionType = iota
	FallsAsleep
	WakesUp
)

func fs2(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	var actions []Action
	sleeps := make(map[int][]int, 0)
	for scanner.Scan() {
		line := scanner.Text()
		action := toAction(line)
		actions = append(actions, action)
		sleeps[action.id] = make([]int, 60)
	}

	sort.Slice(actions, func(i, j int) bool {
		return actions[i].t.Before(actions[j].t)
	})

	id := 0
	for i := 0; i < len(actions); i++ {
		action := actions[i]
		if action.actionType == BeginShift {
			id = action.id
			continue
		}

		start := action.t.Minute()
		end := actions[i+1].t.Minute()
		i++

		for t := start; t < end; t++ {
			sleeps[id][t]++
		}
	}

	maxMinuteAsleep := -1
	chosenId := 0
	chosenMinute := 0
	for id, minutes := range sleeps {
		for minute, total := range minutes {
			if total > maxMinuteAsleep {
				maxMinuteAsleep = total
				chosenId = id
				chosenMinute = minute
			}
		}
	}

	return chosenMinute * chosenId
}

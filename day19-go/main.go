package main

import (
	"bufio"
	"fmt"
	"io"
	"strconv"
	"strings"
	"time"
)

func fs1(input io.Reader, minute int) (int, error) {
	scanner := bufio.NewScanner(input)
	sum := 0
	for scanner.Scan() {
		line := scanner.Text()
		blueprint, err := toBlueprints(line)
		if err != nil {
			return 0, err
		}

		cache = make(map[int]map[State]int, minute)
		d := duration()
		v := best(&blueprint, &State{nbOreRobot: 1}, minute)
		fmt.Println(blueprint.id, v, d())
		sum += blueprint.id * v
	}

	return sum, nil
}

func duration() func() time.Duration {
	start := time.Now()
	return func() time.Duration {
		return time.Since(start)
	}
}

var cache map[int]map[State]int

func getCache(left int, state *State) (int, bool) {
	v, exists := cache[left]
	if !exists {
		return 0, false
	}

	v2, exists := v[*state]
	return v2, exists
}

func addCache(left int, state *State, best int) {
	v, exists := cache[left]
	if !exists {
		v = make(map[State]int)
		cache[left] = v
	}

	v[*state] = best
}

func best(blueprint *Blueprint, state *State, left int) int {
	if left == 0 {
		return state.nbGeode
	}

	if v, exists := getCache(left, state); exists {
		return v
	}

	v := 0

	newState, canBuild := newGeodeRobot(blueprint, state)
	if canBuild {
		s := newState.combineGeode()
		v = max(v, best(blueprint, &s, left-1))
		addCache(left, state, v)
		return v
	}

	newState, canBuild = newObsidianRobot(blueprint, state)
	if canBuild {
		s := newState.combineObsidian()
		v = max(v, best(blueprint, &s, left-1))
	}

	newState, canBuild = newClayRobot(blueprint, state)
	if canBuild {
		s := newState.combineClay()
		v = max(v, best(blueprint, &s, left-1))
	}

	newState, canBuild = newOreRobot(blueprint, state)
	if canBuild {
		s := newState.combineOre()
		v = max(v, best(blueprint, &s, left-1))
	}

	s := state.combine()
	v = max(v, best(blueprint, &s, left-1))
	addCache(left, state, v)
	return v
}

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

func newOreRobot(blueprint *Blueprint, state *State) (State, bool) {
	if blueprint.oreOreCost <= state.nbOre {
		s := *state
		s.nbOre -= blueprint.oreOreCost
		s.nbOreRobot++
		return s, true
	}
	return State{}, false
}

func newClayRobot(blueprint *Blueprint, state *State) (State, bool) {
	if blueprint.clayOreCost <= state.nbOre {
		s := *state
		s.nbOre -= blueprint.clayOreCost
		s.nbClayRobot++
		return s, true
	}
	return State{}, false
}

func newObsidianRobot(blueprint *Blueprint, state *State) (State, bool) {
	if blueprint.obsidianOreCost <= state.nbOre &&
		blueprint.obsidianClayCost <= state.nbClay {
		s := *state
		s.nbOre -= blueprint.obsidianOreCost
		s.nbClay -= blueprint.obsidianClayCost
		s.nbObsidianRobot++
		return s, true
	}
	return State{}, false
}

func newGeodeRobot(blueprint *Blueprint, state *State) (State, bool) {
	if blueprint.geodeOreCost <= state.nbOre &&
		blueprint.geodeObsidianCost <= state.nbObsidian {
		s := *state
		s.nbOre -= blueprint.geodeOreCost
		s.nbObsidian -= blueprint.geodeObsidianCost
		s.nbGeodeRobot++
		return s, true
	}
	return State{}, false
}

func collect(state State) State {
	state.nbOre = state.nbOreRobot
	state.nbClay = state.nbClayRobot
	state.nbObsidian = state.nbObsidianRobot
	state.nbGeode = state.nbGeodeRobot
	return state
}

type State struct {
	nbOre      int
	nbClay     int
	nbObsidian int
	nbGeode    int

	nbOreRobot      int
	nbClayRobot     int
	nbObsidianRobot int
	nbGeodeRobot    int
}

func (s State) combine() State {
	s.nbOre += s.nbOreRobot
	s.nbClay += s.nbClayRobot
	s.nbObsidian += s.nbObsidianRobot
	s.nbGeode += s.nbGeodeRobot
	return s
}

func (s State) combineOre() State {
	s.nbOre += s.nbOreRobot - 1
	s.nbClay += s.nbClayRobot
	s.nbObsidian += s.nbObsidianRobot
	s.nbGeode += s.nbGeodeRobot
	return s
}

func (s State) combineClay() State {
	s.nbOre += s.nbOreRobot
	s.nbClay += s.nbClayRobot - 1
	s.nbObsidian += s.nbObsidianRobot
	s.nbGeode += s.nbGeodeRobot
	return s
}

func (s State) combineObsidian() State {
	s.nbOre += s.nbOreRobot
	s.nbClay += s.nbClayRobot
	s.nbObsidian += s.nbObsidianRobot - 1
	s.nbGeode += s.nbGeodeRobot
	return s
}

func (s State) combineGeode() State {
	s.nbOre += s.nbOreRobot
	s.nbClay += s.nbClayRobot
	s.nbObsidian += s.nbObsidianRobot
	s.nbGeode += s.nbGeodeRobot - 1
	return s
}

type Blueprint struct {
	id                int
	oreOreCost        int
	clayOreCost       int
	obsidianOreCost   int
	obsidianClayCost  int
	geodeOreCost      int
	geodeObsidianCost int
}

func toBlueprints(s string) (Blueprint, error) {
	start := 10
	end := strings.Index(s, ":")
	id, err := strconv.Atoi(s[start:end])
	if err != nil {
		return Blueprint{}, err
	}

	oreOreCost, err := getCost(s, "Each ore robot costs ")
	if err != nil {
		return Blueprint{}, err
	}

	clayOreCost, err := getCost(s, "Each clay robot costs ")
	if err != nil {
		return Blueprint{}, err
	}

	obsidianOreCost, err := getCost(s, "Each obsidian robot costs ")
	if err != nil {
		return Blueprint{}, err
	}

	obsidianClayCost, err := getCost(s, " ore and ")
	if err != nil {
		return Blueprint{}, err
	}

	geodeOreCost, err := getCost(s, "Each geode robot costs ")
	if err != nil {
		return Blueprint{}, err
	}

	start = strings.Index(s, "Each geode robot costs ")
	start = strings.Index(s[start:], "ore and ") + start + len("ore and ")
	end = strings.Index(s[start:], " ")
	geodeObsidianCost, err := strconv.Atoi(s[start : start+end])
	if err != nil {
		return Blueprint{}, err
	}

	return Blueprint{
		id:                id,
		oreOreCost:        oreOreCost,
		clayOreCost:       clayOreCost,
		obsidianOreCost:   obsidianOreCost,
		obsidianClayCost:  obsidianClayCost,
		geodeOreCost:      geodeOreCost,
		geodeObsidianCost: geodeObsidianCost,
	}, nil
}

func getCost(s string, sep string) (int, error) {
	start := strings.Index(s, sep)
	end := strings.Index(s[start+len(sep):], " ")
	return strconv.Atoi(s[start+len(sep) : end+start+len(sep)])
}

func fs2(input io.Reader, minute int, remaining int) (int, error) {
	scanner := bufio.NewScanner(input)
	sum := 1
	for scanner.Scan() {
		if remaining == 0 {
			return sum, nil
		}
		line := scanner.Text()
		blueprint, err := toBlueprints(line)
		if err != nil {
			return 0, err
		}

		cache = make(map[int]map[State]int, minute)
		d := duration()
		v := best(&blueprint, &State{nbOreRobot: 1}, minute)
		fmt.Println(blueprint.id, v, d())
		sum *= v
	}

	return sum, nil
}

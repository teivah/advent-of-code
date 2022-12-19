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

		cache = make(map[int]map[State]int, 24)
		d := duration()
		v := best(&blueprint, State{nbOreRobot: 1}, minute)
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

func getCache(left int, state State) (int, bool) {
	v, exists := cache[left]
	if !exists {
		return 0, false
	}

	v2, exists := v[state]
	return v2, exists
}

func addCache(left int, state State, best int) {
	v, exists := cache[left]
	if !exists {
		v = make(map[State]int)
		cache[left] = v
	}

	v[state] = best
}

func best(blueprint *Blueprint, state State, left int) int {
	if left == 0 {
		return state.nbGeode
	}

	if v, exists := getCache(left, state); exists {
		return v
	}

	stateCollect := collect(state)

	v := 0

	newState, canBuild := newOreRobot(blueprint, state)
	if canBuild {
		v = max(v, best(blueprint, newState.combine(stateCollect), left-1))
	}

	newState, canBuild = newClayRobot(blueprint, state)
	if canBuild {
		v = max(v, best(blueprint, newState.combine(stateCollect), left-1))
	}

	newState, canBuild = newObsidianRobot(blueprint, state)
	if canBuild {
		v = max(v, best(blueprint, newState.combine(stateCollect), left-1))
	}

	newState, canBuild = newGeodeRobot(blueprint, state)
	if canBuild {
		v = max(v, best(blueprint, newState.combine(stateCollect), left-1))
	}

	v = max(v, best(blueprint, state.combine(stateCollect), left-1))

	addCache(left, state, v)
	return v
}

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

func newOreRobot(blueprint *Blueprint, state State) (State, bool) {
	if blueprint.oreOreCost <= state.nbOre {
		state.nbOre -= blueprint.oreOreCost
		state.nbOreRobot++
		return state, true
	}
	return State{}, false
}

func newClayRobot(blueprint *Blueprint, state State) (State, bool) {
	if blueprint.clayOreCost <= state.nbOre {
		state.nbOre -= blueprint.clayOreCost
		state.nbClayRobot++
		return state, true
	}
	return State{}, false
}

func newObsidianRobot(blueprint *Blueprint, state State) (State, bool) {
	if blueprint.obsidianOreCost <= state.nbOre &&
		blueprint.obsidianClayCost <= state.nbClay {
		state.nbOre -= blueprint.obsidianOreCost
		state.nbClay -= blueprint.obsidianClayCost
		state.nbObsidianRobot++
		return state, true
	}
	return State{}, false
}

func newGeodeRobot(blueprint *Blueprint, state State) (State, bool) {
	if blueprint.geodeOreCost <= state.nbOre &&
		blueprint.geodeObsidianCost <= state.nbObsidian {
		state.nbOre -= blueprint.geodeOreCost
		state.nbObsidian -= blueprint.geodeObsidianCost
		state.nbGeodeRobot++
		return state, true
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

func (s State) combine(s2 State) State {
	s.nbOre += s2.nbOre
	s.nbClay += s2.nbClay
	s.nbObsidian += s2.nbObsidian
	s.nbGeode += s2.nbGeode
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

func fs2(input io.Reader) (int, error) {
	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
		line := scanner.Text()
		_ = line
	}

	return 0, nil
}

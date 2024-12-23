package main

import (
	"fmt"
	"io"
	"math"

	"github.com/teivah/go-aoc"
)

type direction struct {
	row int
	col int
}

var (
	dirMap = map[rune]direction{
		'^': {-1, 0},
		'v': {1, 0},
		'>': {0, 1},
		'<': {0, -1},
	}
	numericKeypad = map[rune]aoc.Position{
		'7': {0, 0}, '8': {0, 1}, '9': {0, 2},
		'4': {1, 0}, '5': {1, 1}, '6': {1, 2},
		'1': {2, 0}, '2': {2, 1}, '3': {2, 2},
		'0': {3, 1}, 'A': {3, 2},
	}
	directionKeypad = map[rune]aoc.Position{
		'^': {0, 1}, 'A': {0, 2},
		'<': {1, 0}, 'v': {1, 1}, '>': {1, 2},
	}

	revDirectionKeypad = aoc.MapSwitch(directionKeypad)
	revNumericKeypad   = aoc.MapSwitch(numericKeypad)
	minDistanceCache   map[string]int
	pathsCache         map[string][]string
)

func fs(input io.Reader, count int) int {
	minDistanceCache = make(map[string]int)
	pathsCache = make(map[string][]string)

	lines := aoc.ReaderToStrings(input)
	return solve(lines, count)
}

func solve(input []string, depth int) (res int) {
	for _, str := range input {
		res += cost("A"+str, depth) * aoc.StringToInt(str[:len(str)-1])
	}
	return
}

func cost(str string, depth int) (res int) {
	for i := 0; i < len(str)-1; i++ {
		currPairCost := pairCost(rune(str[i]), rune(str[i+1]), numericKeypad, revNumericKeypad, depth)
		res += currPairCost
	}
	return
}

func pairCost(a, b rune, charToIndex map[rune]aoc.Position, indexToChar map[aoc.Position]rune, depth int) int {
	code := 'd'
	if _, ok := charToIndex['0']; ok {
		code = 'n'
	}
	key := fmt.Sprintf("%c%c%c%d", a, b, code, depth)

	if dist, ok := minDistanceCache[key]; ok {
		return dist
	}

	if depth == 0 {
		m := math.MaxInt
		for _, path := range allPaths(a, b, directionKeypad, revDirectionKeypad) {
			m = min(m, len(path))
		}
		return m
	}

	paths := allPaths(a, b, charToIndex, indexToChar)
	m := math.MaxInt
	for _, path := range paths {
		path = "A" + path
		currCost := 0
		for i := 0; i < len(path)-1; i++ {
			currCost += pairCost(rune(path[i]), rune(path[i+1]), directionKeypad, revDirectionKeypad, depth-1)
		}
		m = min(m, currCost)
	}

	minDistanceCache[key] = m
	return m
}

func allPaths(a, b rune, charToIndex map[rune]aoc.Position, indexToChar map[aoc.Position]rune) []string {
	key := fmt.Sprintf("%c %c", a, b)
	if paths, ok := pathsCache[key]; ok {
		return paths
	}
	var paths []string
	dfs(charToIndex[a], charToIndex[b], []rune{}, charToIndex, indexToChar, make(map[aoc.Position]bool), &paths)
	pathsCache[key] = paths
	return paths
}

func dfs(curr, end aoc.Position, path []rune, charToIndex map[rune]aoc.Position, indexToChar map[aoc.Position]rune, visited map[aoc.Position]bool, allPaths *[]string) {
	if curr == end {
		*allPaths = append(*allPaths, string(path)+"A")
		return
	}
	visited[curr] = true
	for char, dir := range dirMap {
		nIdx := aoc.NewPosition(curr.Row+dir.row, curr.Col+dir.col)
		if _, ok := indexToChar[nIdx]; ok && !visited[nIdx] {
			newPath := aoc.SliceCopy(path)
			dfs(nIdx, end, append(newPath, char), charToIndex, indexToChar, visited, allPaths)
		}
	}
	visited[curr] = false
}

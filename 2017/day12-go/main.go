package main

import (
	"bufio"
	lib "github.com/teivah/advent-of-code"
	"io"
	"strconv"
	"strings"
)

func fs1(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	pipes := make(map[int][]int)
	for scanner.Scan() {
		pipe := toPipe(scanner.Text())
		pipes[pipe.a] = pipe.b
	}

	return countMembers(0, pipes, make(map[int]struct{}))
}

func countMembers(cur int, pipes map[int][]int, visited map[int]struct{}) int {
	sum := 0

	if _, exists := visited[cur]; exists {
		return 0
	}
	sum++
	visited[cur] = struct{}{}

	for _, child := range pipes[cur] {
		sum += countMembers(child, pipes, visited)
	}
	return sum
}

type Pipe struct {
	a int
	b []int
}

func toPipe(s string) Pipe {
	spaces := lib.IndexAll(s, " ")
	return Pipe{
		a: GetInt(s, spaces, 0),
		b: StringsToInts(strings.Split(s[spaces[1]+1:], ", ")),
	}
}

func GetString(s string, del []int, i int) string {
	if i == 0 {
		return s[:del[0]]
	}

	if i == len(del) {
		return s[del[len(del)-1]+1:]
	}

	return s[del[i-1]+1 : del[i]]
}

func GetInt(s string, del []int, i int) int {
	return StringToInt(GetString(s, del, i))
}

func StringToInt(s string) int {
	i, err := strconv.Atoi(s)
	if err != nil {
		panic(err)
	}
	return i
}

func StringsToInts(s []string) []int {
	res := make([]int, len(s))
	for i, v := range s {
		res[i] = StringToInt(v)
	}
	return res
}

func fs2(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	pipes := make(map[int][]int)
	for scanner.Scan() {
		pipe := toPipe(scanner.Text())
		pipes[pipe.a] = pipe.b
	}

	sum := 0
	visited := make(map[int]struct{})
	for k := range pipes {
		sum += countGroups(k, pipes, visited)
	}

	return sum
}

func countGroups(cur int, pipes map[int][]int, visited map[int]struct{}) int {
	if _, exists := visited[cur]; exists {
		return 0
	}
	visited[cur] = struct{}{}
	for _, child := range pipes[cur] {
		countMembers(child, pipes, visited)
	}
	return 1
}

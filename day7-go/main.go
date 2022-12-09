package main

import (
	"bufio"
	"io"
	"math"
	"strconv"
	"strings"
)

type node struct {
	parent    *node
	children  map[string]*node
	size      int
	totalSize int
}

func newNode(parent *node) *node {
	return &node{
		parent:   parent,
		children: make(map[string]*node),
	}
}

func (n *node) addChild(name string, node *node) {
	n.children[name] = node
}

func fs1(input io.Reader) (int, error) {
	root := node{
		children: make(map[string]*node),
	}
	current := &root

	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
		line := scanner.Text()

		if line == "$ cd /" {
			current = &root
		} else if line == "$ cd .." {
			current = current.parent
		} else if strings.HasPrefix(line, "$ cd") {
			current = current.children[line[5:]]
		} else if line == "$ ls" {

		} else {
			// ls
			if strings.HasPrefix(line, "dir") {
				dir := line[4:]
				n := newNode(current)
				current.addChild(dir, n)
			} else {
				split := strings.Split(line, " ")
				size, err := strconv.Atoi(split[0])
				if err != nil {
					return 0, err
				}
				current.size += size
				current.totalSize += size
			}
		}
	}

	_, res := dfs(&root)
	return res, nil
}

func dfs(current *node) (size int, res int) {
	if len(current.children) == 0 {
		if current.size <= 100000 {
			return current.size, current.size
		}
		return current.size, 0
	}

	for _, child := range current.children {
		size, r := dfs(child)
		current.totalSize += size
		res += r
	}

	if current.totalSize <= 100000 {
		return current.totalSize, current.totalSize + res
	}
	return current.totalSize, res
}

const availableMemory = 70000000
const minUnusedSpace = 30000000

func fs2(input io.Reader) (int, error) {
	root := node{
		children: make(map[string]*node),
	}
	current := &root

	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
		line := scanner.Text()

		if line == "$ cd /" {
			current = &root
		} else if line == "$ cd .." {
			current = current.parent
		} else if strings.HasPrefix(line, "$ cd") {
			current = current.children[line[5:]]
		} else if line == "$ ls" {

		} else {
			// ls
			if strings.HasPrefix(line, "dir") {
				dir := line[4:]
				n := newNode(current)
				current.addChild(dir, n)
			} else {
				split := strings.Split(line, " ")
				size, err := strconv.Atoi(split[0])
				if err != nil {
					return 0, err
				}
				current.size += size
				current.totalSize += size
			}
		}
	}

	_, _ = dfs(&root)

	return getBest(&root, availableMemory-root.totalSize), nil
}

func getBest(current *node, unusedSpace int) int {
	best := math.MaxInt
	if current.totalSize+unusedSpace > minUnusedSpace {
		best = current.totalSize
	}

	for _, child := range current.children {
		v := getBest(child, unusedSpace)
		if v < best {
			best = v
		}
	}
	return best
}

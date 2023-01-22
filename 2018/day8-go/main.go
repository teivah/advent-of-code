package main

import (
	"io"

	lib "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	s := lib.ReaderToString(input)

	var numbers []int
	for i := 0; i < len(s); i++ {
		j := i + 1
		for ; j < len(s); j++ {
			if s[j] >= '0' && s[j] <= '9' {
				continue
			}
			break
		}
		numbers = append(numbers, lib.StringToInt(s[i:j]))
		i = j
	}

	root, _ := toNode(numbers, 0)

	return sum(*root)
}

func sum(node Node) int {
	v := node.metadataSum

	for _, child := range node.children {
		v += sum(child)
	}
	return v
}

type Node struct {
	childNodes      int
	metadata        int
	children        []Node
	metadataSum     int
	metadataEntries []int
}

func toNode(numbers []int, i int) (*Node, int) {
	if i >= len(numbers) {
		return nil, i
	}

	childNodes := numbers[i]
	metadata := numbers[i+1]
	node := Node{childNodes: childNodes, metadata: metadata}
	i += 2
	for j := 0; j < childNodes; j++ {
		child, newI := toNode(numbers, i)
		if child == nil {
			continue
		}
		node.children = append(node.children, *child)
		i = newI
	}

	metadataSum := 0
	for j := 0; j < metadata; j++ {
		metadataSum += numbers[i]
		node.metadataEntries = append(node.metadataEntries, numbers[i])
		i++
	}

	node.metadataSum = metadataSum

	return &node, i
}

func fs2(input io.Reader) int {
	s := lib.ReaderToString(input)

	var numbers []int
	for i := 0; i < len(s); i++ {
		j := i + 1
		for ; j < len(s); j++ {
			if s[j] >= '0' && s[j] <= '9' {
				continue
			}
			break
		}
		numbers = append(numbers, lib.StringToInt(s[i:j]))
		i = j
	}

	root, _ := toNode(numbers, 0)

	return value(*root)
}

func value(node Node) int {
	if node.childNodes == 0 {
		return node.metadataSum
	}

	sum := 0
	for _, entry := range node.metadataEntries {
		entry--
		if entry >= node.childNodes {
			continue
		}
		sum += value(node.children[entry])
	}
	return sum
}

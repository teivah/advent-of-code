package aoc_test

import (
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	aoc "github.com/teivah/advent-of-code"
)

func TestGraph(t *testing.T) {
	a := &aoc.DAGNode[string, int]{
		Id:   "a",
		Data: 1,
	}
	b := &aoc.DAGNode[string, int]{
		Id:   "b",
		Data: 2,
	}
	c := &aoc.DAGNode[string, int]{
		Id:   "c",
		Data: 3,
	}

	g := aoc.NewDAG[string, int]()
	g.AddEdge(a, b)
	g.AddEdge(a, c)

	node, contains := g.Node("a")
	require.Equal(t, true, contains)
	assert.Equal(t, "a", node.Id)

	edges := g.Edges("a")
	m := make(map[string]struct{})
	for _, child := range edges {
		m[child.Id] = struct{}{}
	}
	assert.Equal(t, map[string]struct{}{
		"b": {},
		"c": {},
	}, m)
}

func TestGraphSimpleEdge(t *testing.T) {
	g := aoc.NewDAG[string, string]()
	g.AddSimpleEdge("a", "b")
	g.AddSimpleEdge("a", "c")
	g.AddSimpleEdge("b", "c")
	g.AddSimpleEdge("c", "d")
	assert.Equal(t, []string{"a", "b", "c", "d"}, g.TopologicalSort())
}

func TestTopologicalSort(t *testing.T) {
	vertices := []string{"a", "b", "c", "d"}
	children := map[string][]string{
		"a": {"b", "c"},
		"b": {"c"},
		"c": {"d"},
	}
	v := aoc.TopologicalSort(vertices, func(s string) []string {
		return children[s]
	})
	assert.Equal(t, []string{"a", "b", "c", "d"}, v)
}

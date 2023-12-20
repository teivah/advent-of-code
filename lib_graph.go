package aoc

// DAGNode is a DAG node.
type DAGNode[K comparable, V any] struct {
	Id   K
	Data V
}

// DAG is the representation of a directed acyclic graph.
type DAG[K comparable, V any] struct {
	vertices map[K]*DAGNode[K, V]
	edges    map[K][]*DAGNode[K, V]
}

// NewDAG returns a new DAG.
func NewDAG[K comparable, V any]() DAG[K, V] {
	return DAG[K, V]{
		vertices: make(map[K]*DAGNode[K, V]),
		edges:    make(map[K][]*DAGNode[K, V]),
	}
}

// AddSimpleEdge adds a simple edge, without the DAGNode.
func (g DAG[K, V]) AddSimpleEdge(from, to K) {
	g.AddEdge(&DAGNode[K, V]{Id: from}, &DAGNode[K, V]{Id: to})
}

// AddEdge adds a new edge.
func (g DAG[K, V]) AddEdge(from, to *DAGNode[K, V]) {
	if _, exists := g.vertices[from.Id]; !exists {
		g.vertices[from.Id] = from
	}
	if _, exists := g.vertices[to.Id]; !exists {
		g.vertices[to.Id] = to
	}
	g.edges[from.Id] = append(g.edges[from.Id], to)
}

// Node returns a node from an id.
func (g DAG[K, V]) Node(id K) (*DAGNode[K, V], bool) {
	v, exists := g.vertices[id]
	return v, exists
}

// Edges returns the children from an id.
func (g DAG[K, V]) Edges(id K) []*DAGNode[K, V] {
	return g.edges[id]
}

// TopologicalSort applies the topological sort algorithm on a DAG.
func (g DAG[K, V]) TopologicalSort() []K {
	return TopologicalSort(MapKeysToSlice(g.vertices), func(k K) []K {
		v := g.edges[k]
		children := make([]K, 0, len(v))
		for _, child := range v {
			children = append(children, child.Id)
		}
		return children
	})
}

// TopologicalSort applies the topological sort algorithm based on vertices and
// a way to compute the edges.
func TopologicalSort[K comparable](vertices []K, edgesFunc func(K) []K) []K {
	inDegree := make(map[K]int)

	for _, vertex := range vertices {
		inDegree[vertex] = 0
	}

	for _, vertex := range vertices {
		edges := edgesFunc(vertex)
		for _, child := range edges {
			inDegree[child]++
		}
	}

	// TODO
	q := []K{}
	for vertex, factor := range inDegree {
		if factor == 0 {
			q = append(q, vertex)
		}
	}

	res := make([]K, 0, len(vertices))
	for len(q) != 0 {
		vertex := q[0]
		q = q[1:]
		res = append(res, vertex)

		for _, child := range edgesFunc(vertex) {
			inDegree[child]--
			if inDegree[child] == 0 {
				q = append(q, child)
			}
		}
	}

	if len(res) != len(vertices) {
		panic("graph error")
	}
	return res
}

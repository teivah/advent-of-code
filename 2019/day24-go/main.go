package main

import (
	"io"
	"math"

	lib "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	grid := toGrid(lib.ReaderToStrings(input))

	set := make(map[int]struct{})
	for i := 0; ; i++ {
		grid.round()
		rating := grid.biodiversityRating()
		if _, exists := set[rating]; exists {
			return rating
		}
		set[rating] = struct{}{}
	}
}

type Grid struct {
	board [][]bool

	rec *Rec
}

func (g *Grid) String() string {
	s := ""
	for row := 0; row < 5; row++ {
		for col := 0; col < 5; col++ {
			if g.isBug(lib.Position{row, col}) {
				s += "#"
			} else {
				s += "."
			}
		}
		s += "\n"
	}
	return s
}

func (g *Grid) biodiversityRating() int {
	n := 0
	sum := 0
	for row := 0; row < 5; row++ {
		for col := 0; col < 5; col++ {
			if g.isBug(lib.Position{row, col}) {
				sum += int(math.Pow(2., float64(n)))
			}
			n++
		}
	}
	return sum
}

func (g *Grid) round() {
	res := make([][]bool, 5)
	for i := 0; i < 5; i++ {
		res[i] = make([]bool, 5)
	}

	for row := 0; row < 5; row++ {
		for col := 0; col < 5; col++ {
			res[row][col] = g.toBug(lib.Position{row, col})
		}
	}

	g.board = res
}

func (g *Grid) toBug(pos lib.Position) bool {
	adjacents := []lib.Position{
		pos.Delta(-1, 0),
		pos.Delta(1, 0),
		pos.Delta(0, -1),
		pos.Delta(0, 1),
	}
	sum := 0
	for _, p := range adjacents {
		if g.isBug(p) {
			sum++
		}
	}

	if g.isBug(pos) {
		return sum == 1
	} else {
		return sum == 1 || sum == 2
	}
}

func (g *Grid) isBug(pos lib.Position) bool {
	if pos.Row < 0 || pos.Col < 0 || pos.Row >= 5 || pos.Col >= 5 {
		return false
	}
	return g.get(pos)
}

func (g *Grid) get(pos lib.Position) bool {
	return g.board[pos.Row][pos.Col]
}

func toGrid(lines []string) *Grid {
	var board [][]bool
	for _, line := range lines {
		var row []bool
		for i := 0; i < len(line); i++ {
			if line[i] == '#' {
				row = append(row, true)
			} else {
				row = append(row, false)
			}
		}
		board = append(board, row)
	}
	return &Grid{
		board: board,
	}
}

func fs2(input io.Reader, minutes int) int {
	grid := toGridRec(lib.ReaderToStrings(input))

	rec := grid.rec
	for i := 0; i < minutes; i++ {
		rec = rec.round()
	}

	return rec.countBugs()
}

func toGridRec(lines []string) *Grid {
	var board [][]bool
	for _, line := range lines {
		var row []bool
		for i := 0; i < len(line); i++ {
			if line[i] == '#' {
				row = append(row, true)
			} else {
				row = append(row, false)
			}
		}
		board = append(board, row)
	}
	return &Grid{
		rec: &Rec{board: board},
	}
}

type Rec struct {
	inside  *Rec
	outside *Rec
	board   [][]bool
}

func newBoard() [][]bool {
	res := make([][]bool, 5)
	for i := 0; i < 5; i++ {
		res[i] = make([]bool, 5)
	}
	return res
}

func containsBug(board [][]bool) bool {
	for row := 0; row < 5; row++ {
		for col := 0; col < 5; col++ {
			if board[row][col] {
				return true
			}
		}
	}
	return false
}

func (r *Rec) round() *Rec {
	res := make([][]bool, 5)
	for i := 0; i < 5; i++ {
		res[i] = make([]bool, 5)
	}

	if containsBug(r.board) {
		if r.outside == nil {
			r.outside = &Rec{
				inside: r,
				board:  newBoard(),
			}
			r.outside.round()
			return r.outside
		}
		if r.inside == nil {
			r.inside = &Rec{
				outside: r,
				board:   newBoard(),
			}
		}
	}

	for row := 0; row < 5; row++ {
		for col := 0; col < 5; col++ {
			if row == 2 && col == 2 {
				continue
			}
			res[row][col] = r.isBugRec(lib.Position{row, col})
		}
	}

	if r.inside != nil {
		_ = r.inside.round()
	}
	r.board = res

	return r
}

func (r *Rec) get(pos lib.Position) int {
	if pos.Row < 0 || pos.Col < 0 || pos.Row >= 5 || pos.Col >= 5 {
		return 0
	}

	if r.board[pos.Row][pos.Col] {
		return 1
	}
	return 0
}

func (r *Rec) countBugs() int {
	sum := 0
	for row := 0; row < 5; row++ {
		for col := 0; col < 5; col++ {
			if row == 2 && col == 2 {
				continue
			}
			if r.board[row][col] {
				sum++
			}
		}
	}

	if r.inside != nil {
		sum += r.inside.countBugs()
	}
	return sum
}

func (r *Rec) getOutside(pos lib.Position) int {
	if r.outside == nil {
		return 0
	}
	return r.outside.get(pos)
}

func (r *Rec) getInside(pos lib.Position) int {
	if r.inside == nil {
		return 0
	}
	return r.inside.get(pos)
}

/*
   A B C D E
   F G H I J
   K L x N O
   P Q R S T
   U V W X Y
*/
func (r *Rec) sumAdjacent(pos lib.Position) int {
	sum := 0

	// 4 "normal"
	sum += r.get(pos.Delta(-1, 0))
	sum += r.get(pos.Delta(1, 0))
	sum += r.get(pos.Delta(0, -1))
	sum += r.get(pos.Delta(0, 1))

	if pos.Row == 0 {
		sum += r.getOutside(H)
	}
	if pos.Row == 4 {
		sum += r.getOutside(R)
	}
	if pos.Col == 0 {
		sum += r.getOutside(L)
	}
	if pos.Col == 4 {
		sum += r.getOutside(N)
	}

	if pos == H {
		sum += r.getInside(A) +
			r.getInside(B) +
			r.getInside(C) +
			r.getInside(D) +
			r.getInside(E)
	}

	if pos == R {
		sum += r.getInside(U) +
			r.getInside(V) +
			r.getInside(W) +
			r.getInside(X) +
			r.getInside(Y)
	}

	if pos == L {
		sum += r.getInside(A) +
			r.getInside(F) +
			r.getInside(K) +
			r.getInside(P) +
			r.getInside(U)
	}

	if pos == N {
		sum += r.getInside(E) +
			r.getInside(J) +
			r.getInside(O) +
			r.getInside(T) +
			r.getInside(Y)
	}

	return sum
}

var (
	A = lib.Position{0, 0}
	B = lib.Position{0, 1}
	C = lib.Position{0, 2}
	D = lib.Position{0, 3}
	E = lib.Position{0, 4}
	F = lib.Position{1, 0}
	G = lib.Position{1, 1}
	H = lib.Position{1, 2}
	I = lib.Position{1, 3}
	J = lib.Position{1, 4}
	K = lib.Position{2, 0}
	L = lib.Position{2, 1}
	N = lib.Position{2, 3}
	O = lib.Position{2, 4}
	P = lib.Position{3, 0}
	Q = lib.Position{3, 1}
	R = lib.Position{3, 2}
	S = lib.Position{3, 3}
	T = lib.Position{3, 4}
	U = lib.Position{4, 0}
	V = lib.Position{4, 1}
	W = lib.Position{4, 2}
	X = lib.Position{4, 3}
	Y = lib.Position{4, 4}
)

func (r *Rec) isBugRec(pos lib.Position) bool {
	sum := r.sumAdjacent(pos)
	if r.board[pos.Row][pos.Col] {
		return sum == 1
	}
	return sum == 1 || sum == 2
}

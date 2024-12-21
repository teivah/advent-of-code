package main

import (
	"fmt"
	"io"
	"os"
	"sort"
	"strings"

	"github.com/teivah/go-aoc"
)

type cell uint8

const (
	track cell = iota
	wall
)

func fs1(input io.Reader) int {
	var start, end aoc.Position
	board := aoc.NewBoardFromReader(input, func(row, col int, r rune) cell {
		switch r {
		default:
			panic(r)
		case '.':
			return track
		case '#':
			return wall
		case 'S':
			start = aoc.NewPosition(row, col)
			return track
		case 'E':
			end = aoc.NewPosition(row, col)
			return track
		}
	})
	shortest := shortestPath(board, start, end)

	//all := make(map[aoc.Position]int)
	//for pos, c := range board.Positions {
	//	if c != track {
	//		continue
	//	}
	//	all[pos] = shortestPathWithAll(board, pos, end)
	//}
	//offload(all)
	all := load()
	return countCheats(all, board, start, end, shortest)
}

func load() map[aoc.Position]int {
	f, err := os.Open("all.txt")
	if err != nil {
		panic(err)
	}
	lines := aoc.ReaderToStrings(f)
	m := make(map[aoc.Position]int)
	for _, line := range lines {
		var a, b, value int
		_, err := fmt.Sscanf(line, "%d,%d=%d", &a, &b, &value)
		if err != nil {
			panic(err)
		}
		m[aoc.NewPosition(a, b)] = value
	}
	return m
}

func offload(all map[aoc.Position]int) {
	var lines []string
	for pos, v := range all {
		lines = append(lines, fmt.Sprintf("%d,%d=%d", pos.Row, pos.Col, v))
	}
	file, err := os.Create("all.txt")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	_, err = file.WriteString(strings.Join(lines, "\n"))
	if err != nil {
		panic(err)
	}
}

func shortestPathWithAll(board aoc.Board[cell], start, end aoc.Position) int {
	q := []state1{
		{pos: start},
	}
	visited := make(map[aoc.Position]bool)

	for len(q) != 0 {
		s := q[0]
		q = q[1:]
		if s.pos == end {
			return s.moves
		}

		if visited[s.pos] {
			continue
		}
		visited[s.pos] = true
		if !board.Contains(s.pos) {
			continue
		}
		if board.Get(s.pos) == wall {
			continue
		}

		moves := s.moves + 1
		q = append(q, state1{moves: moves, pos: s.pos.Move(aoc.Up, 1)})
		q = append(q, state1{moves: moves, pos: s.pos.Move(aoc.Down, 1)})
		q = append(q, state1{moves: moves, pos: s.pos.Move(aoc.Left, 1)})
		q = append(q, state1{moves: moves, pos: s.pos.Move(aoc.Right, 1)})
	}
	return -1
}

type state1 struct {
	moves int
	pos   aoc.Position
}

func shortestPath(board aoc.Board[cell], start, end aoc.Position) int {
	q := []state1{
		{pos: start},
	}
	visited := make(map[aoc.Position]bool)

	for len(q) != 0 {
		s := q[0]
		q = q[1:]
		if s.pos == end {
			return s.moves
		}

		if visited[s.pos] {
			continue
		}
		visited[s.pos] = true
		if !board.Contains(s.pos) {
			continue
		}
		if board.Get(s.pos) == wall {
			continue
		}

		moves := s.moves + 1
		q = append(q, state1{moves: moves, pos: s.pos.Move(aoc.Up, 1)})
		q = append(q, state1{moves: moves, pos: s.pos.Move(aoc.Down, 1)})
		q = append(q, state1{moves: moves, pos: s.pos.Move(aoc.Left, 1)})
		q = append(q, state1{moves: moves, pos: s.pos.Move(aoc.Right, 1)})
	}
	return -1
}

type state2 struct {
	moves      int
	pos        aoc.Position
	cheated    bool
	cheatedPos aoc.Position
}

type key struct {
	pos     aoc.Position
	cheated bool
	cheat   aoc.Position
}

func countCheats(all map[aoc.Position]int, board aoc.Board[cell], start, end aoc.Position, shortest int) int {
	q := []state2{
		{pos: start},
	}
	visited := make(map[key]bool)

	res := 0
	for len(q) != 0 {
		s := q[0]
		q = q[1:]
		if s.pos == end {
			if s.cheated {
				saved := shortest - s.moves
				if saved >= 100 {
					res++
				}
			}
			continue
		}

		if s.moves >= shortest {
			continue
		}

		k := key{pos: s.pos, cheated: s.cheated, cheat: s.cheatedPos}
		if visited[k] {
			continue
		}
		visited[k] = true
		if !board.Contains(s.pos) {
			continue
		}
		if board.Get(s.pos) == wall {
			if s.cheated {
				continue
			}
			s.cheated = true
			s.cheatedPos = s.pos
		} else {
			if s.cheated {
				solution := s.moves + all[s.pos]

				saved := shortest - solution
				if saved >= 100 {
					res++
				}
				continue
			}
		}

		moves := s.moves + 1
		q = append(q, state2{cheated: s.cheated, cheatedPos: s.cheatedPos, moves: moves, pos: s.pos.Move(aoc.Up, 1)})
		q = append(q, state2{cheated: s.cheated, cheatedPos: s.cheatedPos, moves: moves, pos: s.pos.Move(aoc.Down, 1)})
		q = append(q, state2{cheated: s.cheated, cheatedPos: s.cheatedPos, moves: moves, pos: s.pos.Move(aoc.Left, 1)})
		q = append(q, state2{cheated: s.cheated, cheatedPos: s.cheatedPos, moves: moves, pos: s.pos.Move(aoc.Right, 1)})
	}
	return res
}

func fs2(input io.Reader, minSaves int) int {
	var start, end aoc.Position
	board := aoc.NewBoardFromReader(input, func(row, col int, r rune) cell {
		switch r {
		default:
			panic(r)
		case '.':
			return track
		case '#':
			return wall
		case 'S':
			start = aoc.NewPosition(row, col)
			return track
		case 'E':
			end = aoc.NewPosition(row, col)
			return track
		}
	})
	shortest := shortestPath(board, start, end)

	//all := make(map[aoc.Position]int)
	//for pos, c := range board.Positions {
	//	if c != track {
	//		continue
	//	}
	//	all[pos] = shortestPathWithAll(board, pos, end)
	//}
	_ = shortest
	//offload(all)
	all := load()

	//cur := start
	//visited := make(map[aoc.Position]bool)
	//log := make(map[int]int)
	//solutions := make(map[Solution]bool)
	//for i := 0; ; i++ {
	//	count(log, all, board, cur, i, shortest, minSaves, end, solutions)
	//	visited[cur] = true
	//	cur = getNext(visited, board, cur)
	//	if cur == end {
	//		break
	//	}
	//}
	////ks := aoc.MapKeysToSlice(log)
	////sort.Slice(ks, func(i, j int) bool {
	////	return ks[j] > ks[i]
	////})
	////for _, k := range ks {
	////	fmt.Printf("There are %d that save %d\n", log[k], k)
	////}
	//
	//return len(solutions)

	uniqueCheats := make(map[cheat]int)
	count := 0

	for p1, d1 := range all {
		for p2, d2 := range all {
			if d2-d1-p1.Manhattan(p2) >= minSaves && p1.Manhattan(p2) <= 20 {
				uniqueCheats[cheat{start: p1, end: p2}] = d2 - d1
				count++
			}
		}
	}
	return count
}

type Cheat struct {
	start aoc.Position
	end   aoc.Position
}

func getNext(visited map[aoc.Position]bool, board aoc.Board[cell], pos aoc.Position) aoc.Position {
	for _, dir := range []aoc.Direction{aoc.Up, aoc.Down, aoc.Left, aoc.Right} {
		p := pos.Move(dir, 1)
		if !board.Contains(p) {
			continue
		}
		if board.Get(p) == track && !visited[p] {
			return p
		}
	}
	panic(pos)
}

type state3 struct {
	moves             int
	pos               aoc.Position
	currentlyCheating bool
	cheatingRemaining int
	cheatedPosStart   aoc.Position
	cheatedPosEnd     aoc.Position
	prevPos           aoc.Position
	visited           map[aoc.Position]bool
}

type key2 struct {
	pos               aoc.Position
	currentlyCheating bool
	cheatStart        aoc.Position
	cheatEnd          aoc.Position
}

type cheat struct {
	start aoc.Position
	end   aoc.Position
}

type Key struct {
	cheat cheat
	moves int
}

func display(pos, spos, end aoc.Position, board aoc.Board[cell]) {
	fmt.Println(board.String(func(p aoc.Position, c cell) rune {
		if p == pos {
			return 'A'
		}
		if p == spos {
			return 'B'
		}
		if p == end {
			return 'E'
		}
		switch c {
		default:
			panic(c)
		case wall:
			return '#'
		case track:
			return '.'
		}
	}, ' '))

}

type Solution struct {
	start aoc.Position
	end   aoc.Position
	saved int
}

func count(log map[int]int, all map[aoc.Position]int, board aoc.Board[cell], pos aoc.Position, moves, shortest, minSaves int, end aoc.Position, solutions map[Solution]bool) {
	type state struct {
		pos       aoc.Position
		moves     int
		remaining int
	}
	q := make([]state, 0)
	for _, dir := range []aoc.Direction{aoc.Up, aoc.Down, aoc.Left, aoc.Right} {
		p := pos.Move(dir, 1)
		if !board.Contains(p) {
			panic(p)
		}
		if board.Get(p) == track {
			continue
		}
		q = append(q, state{
			pos:       p,
			moves:     moves + 1,
			remaining: 19,
		})
	}
	visited := make(map[state]bool)
	for len(q) != 0 {
		s := q[0]
		q = q[1:]
		if s.remaining < 0 {
			continue
		}
		if shortest-s.moves < minSaves {
			continue
		}
		if !board.Contains(s.pos) {
			continue
		}
		if visited[s] {
			continue
		}
		visited[s] = true

		if s.pos == end {
			//if !res[end] {
			//	log[shortest-s.moves]++
			//}
			solutions[Solution{start: pos, end: end, saved: shortest - s.moves}] = true
		} else if board.Get(s.pos) == wall {
			for _, dir := range []aoc.Direction{aoc.Up, aoc.Down, aoc.Left, aoc.Right} {
				q = append(q, state{pos: s.pos.Move(dir, 1), moves: s.moves + 1, remaining: s.remaining - 1})
			}
		} else if board.Get(s.pos) == track {
			solution := s.moves + all[s.pos]
			saved := shortest - solution
			if saved >= minSaves {
				//if !res[s.pos] {
				//	log[saved]++
				//}
				solutions[Solution{start: pos, end: s.pos, saved: saved}] = true
			}
		} else {
			panic("what?")
		}
	}
}

func countCheats2(all map[aoc.Position]int, board aoc.Board[cell], start, end aoc.Position, shortest int) int {
	q := []state3{{pos: start, visited: map[aoc.Position]bool{}}}
	visited := make(map[key2]bool)
	minSaves := 50

	res := make(map[cheat]bool)
	count := make(map[int]int)
	for len(q) != 0 {
		s := q[0]
		q = q[1:]
		if s.visited[s.pos] {
			continue
		}
		s.visited[s.pos] = true
		if s.pos == end {
			if s.currentlyCheating {
				saved := shortest - s.moves
				if saved >= minSaves {
					c := cheat{start: s.cheatedPosStart, end: s.pos}
					if !res[c] {
						//if saved == 72 {
						//	fmt.Println(board.String(func(pos aoc.Position, c cell) rune {
						//		if s.visited[pos] {
						//			return 'x'
						//		}
						//		switch c {
						//		default:
						//			panic(c)
						//		case wall:
						//			return '#'
						//		case track:
						//			return '.'
						//		}
						//	}, ' '))
						//}
						res[c] = true
						count[saved]++
					}
				}
			}
			continue
		}

		if s.moves >= shortest {
			continue
		}

		//if !s.currentlyCheating {
		k := key2{pos: s.pos, currentlyCheating: s.currentlyCheating, cheatStart: s.cheatedPosStart, cheatEnd: s.cheatedPosEnd}
		if visited[k] {
			continue
		}
		visited[k] = true
		//}
		if !board.Contains(s.pos) {
			continue
		}
		if board.Get(s.pos) == wall {
			if s.currentlyCheating {
				s.cheatingRemaining--
				if s.cheatingRemaining < 0 {
					continue
				}
			} else {
				s.currentlyCheating = true
				s.cheatingRemaining = 19
				s.cheatedPosStart = s.prevPos
			}
		} else if s.currentlyCheating {
			solution := s.moves + all[s.pos]
			saved := shortest - solution
			if saved >= minSaves {
				c := cheat{start: s.cheatedPosStart, end: s.pos}
				if !res[c] {
					//if saved == 72 {
					//	fmt.Println(board.String(func(pos aoc.Position, c cell) rune {
					//		if s.visited[pos] {
					//			return 'x'
					//		}
					//		switch c {
					//		default:
					//			panic(c)
					//		case wall:
					//			return '#'
					//		case track:
					//			return '.'
					//		}
					//	}, ' '))
					//}
					res[c] = true
					count[saved]++
				}
			}
			continue
		}

		s.moves++
		pos := s.pos
		s.prevPos = pos

		s.pos = pos.Move(aoc.Up, 1)
		s.visited = aoc.MapCopy(s.visited)
		q = append(q, s)
		s.pos = pos.Move(aoc.Down, 1)
		s.visited = aoc.MapCopy(s.visited)
		q = append(q, s)
		s.pos = pos.Move(aoc.Left, 1)
		s.visited = aoc.MapCopy(s.visited)
		q = append(q, s)
		s.pos = pos.Move(aoc.Right, 1)
		s.visited = aoc.MapCopy(s.visited)
		q = append(q, s)
	}

	ks := aoc.MapKeysToSlice(count)
	sort.Slice(ks, func(i, j int) bool {
		return ks[j] > ks[i]
	})
	for _, k := range ks {
		fmt.Printf("There are %d that save %d\n", count[k], k)
	}

	return len(res)
}

package day15_go

import (
	"bufio"
	"fmt"
	"io"
)

type Direction int

type Unit int

const (
	Width = 7

	Left  Direction = 0
	Right Direction = 1

	Rock Unit = 0
	Air  Unit = 1
)

type Play interface {
	MoveLeft(t *Tunnel)
	MoveRight(t *Tunnel)
	// IsBlocked?
	Down(t *Tunnel) bool
}

type RockHorizontal struct {
	/*
		>####
		 ^
	*/
	col int
	row int
}

func NewRockHorizontal(t *Tunnel) *RockHorizontal {
	for len(t.rows) <= t.highestRock+4 {
		t.addNewRow()
	}

	t.set(t.highestRock+4, 2, Rock)
	t.set(t.highestRock+4, 3, Rock)
	t.set(t.highestRock+4, 4, Rock)
	t.set(t.highestRock+4, 5, Rock)

	return &RockHorizontal{col: 2, row: t.highestRock + 4}
}

func (r *RockHorizontal) MoveLeft(t *Tunnel) {
	if r.col == 0 {
		return
	}

	if t.get(r.row, r.col-1) == Rock {
		return
	}

	t.set(r.row, r.col-1, Rock)
	t.set(r.row, r.col+3, Air)
	r.col--
}

func (r *RockHorizontal) MoveRight(t *Tunnel) {
	if r.col == Width-4 {
		return
	}

	if t.get(r.row, r.col+4) == Rock {
		return
	}

	t.move(r.row, r.col, 0, 4)
	r.col++
}

func (r *RockHorizontal) Down(t *Tunnel) bool {
	if t.get(r.row-1, r.col) == Air &&
		t.get(r.row-1, r.col+1) == Air &&
		t.get(r.row-1, r.col+2) == Air &&
		t.get(r.row-1, r.col+3) == Air {
		t.set(r.row, r.col, Air)
		t.set(r.row, r.col+1, Air)
		t.set(r.row, r.col+2, Air)
		t.set(r.row, r.col+3, Air)
		t.set(r.row-1, r.col, Rock)
		t.set(r.row-1, r.col+1, Rock)
		t.set(r.row-1, r.col+2, Rock)
		t.set(r.row-1, r.col+3, Rock)
		r.row--
		return false
	}
	return true
}

type RockPlus struct {
	/*
		  #
		>###
		  #
		  ^
	*/
	col int
	row int
}

func NewRockPlus(t *Tunnel) *RockPlus {
	for len(t.rows) <= t.highestRock+6 {
		t.addNewRow()
	}

	t.set(t.highestRock+5, 2, Rock)
	t.set(t.highestRock+5, 3, Rock)
	t.set(t.highestRock+5, 4, Rock)
	t.set(t.highestRock+6, 3, Rock)
	t.set(t.highestRock+4, 3, Rock)

	return &RockPlus{col: 3, row: t.highestRock + 5}
}

func (r *RockPlus) MoveLeft(t *Tunnel) {
	col := r.col
	row := r.row
	if col <= 1 {
		return
	}

	if t.get(row, col-2) == Air &&
		t.get(row+1, col-1) == Air &&
		t.get(row-1, col-1) == Air {
		t.move(row+1, col, 0, -1)
		t.move(row-1, col, 0, -1)
		t.move(row, col+1, 0, -3)
		r.col--
	}
}

func (r *RockPlus) MoveRight(t *Tunnel) {
	col := r.col
	row := r.row
	if col >= Width-2 {
		return
	}

	if t.get(row, col+2) == Air &&
		t.get(row+1, col+1) == Air &&
		t.get(row-1, col+1) == Air {
		t.move(row+1, col, 0, +1)
		t.move(row-1, col, 0, +1)
		t.move(row, col-1, 0, +3)
		r.col++
	}
}

func (r *RockPlus) Down(t *Tunnel) bool {
	row := r.row
	col := r.col
	if t.get(row-2, col) == Air &&
		t.get(row-1, col-1) == Air &&
		t.get(row-1, col+1) == Air {
		t.move(row, col-1, -1, 0)
		t.move(row, col+1, -1, 0)
		t.move(row+1, col, -3, 0)
		r.row--
		return false
	}

	return true
}

type RockL struct {
	/*
			 #
			 #
		   ###<
			 ^
	*/
	col int
	row int
}

func NewRockL(t *Tunnel) *RockL {
	for len(t.rows) <= t.highestRock+6 {
		t.addNewRow()
	}

	t.set(t.highestRock+4, 2, Rock)
	t.set(t.highestRock+4, 3, Rock)
	t.set(t.highestRock+4, 4, Rock)
	t.set(t.highestRock+5, 4, Rock)
	t.set(t.highestRock+6, 4, Rock)

	return &RockL{col: 2, row: t.highestRock + 4}
}

func (r *RockL) MoveLeft(t *Tunnel) {
	row := r.row
	col := r.col

	if col == 0 {
		return
	}

	if t.isFree(row, col-1) &&
		t.isFree(row+1, col+1) &&
		t.isFree(row+2, col+1) {
		t.move(row+1, col+2, 0, -1)
		t.move(row+2, col+2, 0, -1)
		t.move(row, col+2, 0, -3)
		r.col--
	}
}

func (r *RockL) MoveRight(t *Tunnel) {
	row := r.row
	col := r.col

	if col >= Width-3 {
		return
	}

	if t.isFree(row, col+3) &&
		t.isFree(row+1, col+3) &&
		t.isFree(row+2, col+3) {
		t.move(row, col, 0, 3)
		t.move(row+1, col+2, 0, 1)
		t.move(row+2, col+2, 0, 1)
		r.col++
	}
}

func (r *RockL) Down(t *Tunnel) bool {
	row := r.row
	col := r.col

	if t.isFree(row-1, col) &&
		t.isFree(row-1, col+1) &&
		t.isFree(row-1, col+2) {
		t.move(row, col, -1, 0)
		t.move(row, col+1, -1, 0)
		t.move(row+2, col+2, -3, 0)
		r.row--
		return false
	}
	return true
}

type RockVertical struct {
	/*
	 #
	 #
	 #
	 #<
	 ^
	*/
	col int
	row int
}

func NewRockVertical(t *Tunnel) *RockVertical {
	for len(t.rows) <= t.highestRock+7 {
		t.addNewRow()
	}

	t.set(t.highestRock+4, 2, Rock)
	t.set(t.highestRock+5, 2, Rock)
	t.set(t.highestRock+6, 2, Rock)
	t.set(t.highestRock+7, 2, Rock)

	return &RockVertical{col: 2, row: t.highestRock + 4}
}

func (r *RockVertical) MoveLeft(t *Tunnel) {
	row := r.row
	col := r.col

	if col == 0 {
		return
	}

	if t.isFree(row, col-1) &&
		t.isFree(row+1, col-1) &&
		t.isFree(row+2, col-1) &&
		t.isFree(row+3, col-1) {
		t.move(row, col, 0, -1)
		t.move(row+1, col, 0, -1)
		t.move(row+2, col, 0, -1)
		t.move(row+3, col, 0, -1)
		r.col--
	}
}

func (r *RockVertical) MoveRight(t *Tunnel) {
	row := r.row
	col := r.col

	if col == Width-1 {
		return
	}

	if t.isFree(row, col+1) &&
		t.isFree(row+1, col+1) &&
		t.isFree(row+2, col+1) &&
		t.isFree(row+3, col+1) {
		t.move(row, col, 0, 1)
		t.move(row+1, col, 0, 1)
		t.move(row+2, col, 0, 1)
		t.move(row+3, col, 0, 1)
		r.col++
	}
}

func (r *RockVertical) Down(t *Tunnel) bool {
	row := r.row
	col := r.col

	if t.isFree(row-1, col) {
		t.move(row+3, col, -4, 0)
		r.row--
		return false
	}
	return true
}

type RockSquare struct {
	/*
			##
		   >##
		    ^
	*/
	col int
	row int
}

func NewRockSquare(t *Tunnel) *RockSquare {
	for len(t.rows) <= t.highestRock+5 {
		t.addNewRow()
	}

	t.set(t.highestRock+4, 2, Rock)
	t.set(t.highestRock+5, 2, Rock)
	t.set(t.highestRock+4, 3, Rock)
	t.set(t.highestRock+5, 3, Rock)

	return &RockSquare{col: 2, row: t.highestRock + 4}
}

func (r *RockSquare) MoveLeft(t *Tunnel) {
	row := r.row
	col := r.col

	if col == 0 {
		return
	}

	if t.isFree(row, col-1) &&
		t.isFree(row+1, col-1) {
		t.move(row, col+1, 0, -2)
		t.move(row+1, col+1, 0, -2)
		r.col--
	}
}

func (r *RockSquare) MoveRight(t *Tunnel) {
	row := r.row
	col := r.col

	if col == Width-2 {
		return
	}

	if t.isFree(row, col+2) &&
		t.isFree(row+1, col+2) {
		t.move(row, col, 0, 2)
		t.move(row+1, col, 0, 2)
		r.col++
	}
}

func (r *RockSquare) Down(t *Tunnel) bool {
	row := r.row
	col := r.col

	if t.isFree(row-1, col) &&
		t.isFree(row-1, col+1) {
		t.move(row+1, col, -2, 0)
		t.move(row+1, col+1, -2, 0)
		r.row--
		return false
	}
	return true
}

type Tunnel struct {
	rows        []Row
	highestRock int
}

func (t *Tunnel) addNewRow() {
	cols := make([]Unit, Width)
	for i := range cols {
		cols[i] = Air
	}
	t.rows = append(t.rows, Row{cols})
}

func (t *Tunnel) addBaseRow() {
	cols := make([]Unit, Width)
	for i := range cols {
		cols[i] = Rock
	}
	t.highestRock = len(t.rows)
	t.rows = append(t.rows, Row{cols})
}

func (t *Tunnel) get(row, col int) Unit {
	return t.rows[row].cols[col]
}

func (t *Tunnel) isFree(row, col int) bool {
	return t.rows[row].cols[col] == Air
}

func (t *Tunnel) move(row, col, deltaRow, deltaCol int) {
	t.set(row+deltaRow, col+deltaCol, Rock)
	t.set(row, col, Air)
}

func (t *Tunnel) set(row, col int, u Unit) {
	t.rows[row].cols[col] = u
}

func (t *Tunnel) setHighestRock() {
	t.highestRock = 0
	for i, row := range t.rows {
		for _, u := range row.cols {
			if u == Rock {
				t.highestRock = i
				break
			}
		}
	}
}

type Row struct {
	cols []Unit
}

func (t *Tunnel) getNextPiece(turn int) Play {
	switch turn % 5 {
	case 0:
		return NewRockHorizontal(t)
	case 1:
		return NewRockPlus(t)
	case 2:
		return NewRockL(t)
	case 3:
		return NewRockVertical(t)
	case 4:
		return NewRockSquare(t)
	default:
		panic(turn)
	}
}

func (t *Tunnel) print() {
	for row := len(t.rows) - 1; row >= 0; row-- {
		if row == 0 {
			fmt.Println("+-------+")
			continue
		}

		fmt.Print("|")
		for col := 0; col < Width; col++ {
			u := t.get(row, col)
			if u == Air {
				fmt.Print(".")
			} else {
				fmt.Print("#")
			}
		}
		if row == t.highestRock {
			fmt.Printf("| %d <-- highest\n", row)
		} else {
			fmt.Printf("| %d\n", row)
		}

	}
	fmt.Println()
}

const debug = false

func fn1(input io.Reader, moves int) (int, error) {
	directions := toListDirections(input)
	idx := 0

	t := &Tunnel{}
	t.addBaseRow()

	for i := 0; i < moves; i++ {
		if debug {
			fmt.Printf("Turn %d\n", i)
		}
		p := t.getNextPiece(i)
		if debug {
			t.print()
		}

		for {
			direction := directions[idx]
			idx++
			if idx >= len(directions) {
				idx = 0
			}
			if direction == Left {
				if debug {
					fmt.Println("Move left")
				}
				p.MoveLeft(t)
			} else {
				if debug {
					fmt.Println("Move right")
				}
				p.MoveRight(t)
			}
			if debug {
				t.print()
			}

			if debug {
				fmt.Println("Down")
				stop := p.Down(t)
				t.print()
				if stop {
					break
				}
			} else {
				if p.Down(t) {
					break
				}
			}
		}
		t.setHighestRock()
	}

	return t.highestRock, nil
}

func toListDirections(input io.Reader) []Direction {
	var directions []Direction
	scanner := bufio.NewScanner(input)
	scanner.Scan()
	s := scanner.Text()
	for i := 0; i < len(s); i++ {
		c := s[i]
		if c == '<' {
			directions = append(directions, Left)
		} else {
			directions = append(directions, Right)
		}
	}
	return directions
}

func isFull(t *Tunnel) bool {
	row := t.rows[t.highestRock]
	for _, u := range row.cols {
		if u == Air {
			return false
		}
	}
	return true
}

func fn2(input io.Reader, moves int) (int, error) {
	directions := toListDirections(input)
	idx := 0

	t := &Tunnel{}
	t.addBaseRow()

	previous := 0
	for i := 0; i < moves; i++ {
		if i == 1751 {
			previous = t.highestRock
		}

		if i == 3160 {
			fmt.Printf("%v\n", t.highestRock-previous)
			return 0, nil
		}

		p := t.getNextPiece(i)

		for {
			direction := directions[idx]
			idx++
			if idx >= len(directions) {
				idx = 0
			}
			if direction == Left {
				p.MoveLeft(t)
			} else {
				p.MoveRight(t)
			}

			if p.Down(t) {
				break
			}
		}
		t.setHighestRock()
	}

	return t.highestRock, nil
}

/*
From move 1751, every 1755 moves add 2768

1000000000000-1751=999999998249 (we have to add 2726 in the end, the res during the first 1751 moves)

999999998249, we can fit 569800568 times 1755 (but we're missing 999999998249-999999996840=1409)
569800568 * 2768 = 1,577,207,972,224

1,577,207,972,224 + 2726 + how many moves in 1409 ? 2236
*/

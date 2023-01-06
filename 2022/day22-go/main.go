package main

import (
	"bufio"
	"fmt"
	"io"
	"strconv"
)

func fs1(input io.Reader) (int, error) {
	scanner := bufio.NewScanner(input)
	var lines []string
	for scanner.Scan() {
		line := scanner.Text()
		lines = append(lines, line)
	}

	board := toBoard(lines[:len(lines)-2])

	steps := toSteps(lines[len(lines)-1])

	for _, step := range steps {
		if step.isMove {
			for i := 0; i < step.move; i++ {
				isBlocked := board.move()
				if isBlocked {
					break
				}
				//board.print()
			}
		} else {
			if step.turn == Left {
				board.left()
			} else {
				board.right()
			}
			//board.print()
		}
	}

	res := (board.curRow+1)*1000 + (board.curCol+1)*4 + board.getDirectionValue()
	return res, nil
}

type Board struct {
	board  [][]Position
	maxRow int
	maxCol int

	curRow       int
	curCol       int
	curDirection *Direction
	hints        func(cubeSize int, faces map[int]Face, cube [][][]*Position, board [][]Position)
	faces        map[int]Face
	cubeSize     int
	cube         [][][]*Position

	x        int
	y        int
	z        int
	cubeType int
}

type Direction struct {
	direction DirectionType
}

func (d *Direction) left() {
	switch d.direction {
	case Left:
		d.direction = Down
	case Right:
		d.direction = Up
	case Down:
		d.direction = Right
	case Up:
		d.direction = Left
	}
}

func (d *Direction) right() {
	switch d.direction {
	case Left:
		d.direction = Up
	case Right:
		d.direction = Down
	case Down:
		d.direction = Left
	case Up:
		d.direction = Right
	}
}

type DirectionType = int

const (
	Left DirectionType = iota
	Right
	Down
	Up
)

func (b *Board) getDirectionValue() int {
	switch b.curDirection.direction {
	case Left:
		return 2
	case Right:
		return 0
	case Down:
		return 1
	case Up:
		return 3
	default:
		panic("unhandled")
	}
}

func (b *Board) getLeftFace(row, col int) int {
	if col < 0 {
		return 0
	}
	pos := b.board[row][col]
	if pos.posType == Empty {
		return 0
	}
	return pos.face
}

func (b *Board) getRightFace(row, col int) int {
	if col >= b.maxCol {
		return 0
	}
	pos := b.board[row][col]
	if pos.posType == Empty {
		return 0
	}
	return pos.face
}

func (b *Board) getDownFace(row, col int) int {
	if row >= b.maxRow {
		return 0
	}
	pos := b.board[row][col]
	if pos.posType == Empty {
		return 0
	}
	return pos.face
}

func (b *Board) getUpFace(row, col int) int {
	if row < 0 {
		return 0
	}
	pos := b.board[row][col]
	if pos.posType == Empty {
		return 0
	}
	return pos.face
}

func (b *Board) left() {
	b.curDirection.left()
}

func (b *Board) right() {
	b.curDirection.right()
}

// Return whether is blocked
func (b *Board) move() bool {
	p := b.board[b.curRow][b.curCol]
	switch b.curDirection.direction {
	case Left:
		if p.left == nil {
			if b.board[b.curRow][b.curCol-1].posType == Wall {
				return true
			}
			b.curCol--
			return false
		} else {
			left := p.left
			if b.board[left.row][left.col].posType == Wall {
				return true
			}
			b.curCol = left.col
			return false
		}
	case Right:
		if p.right == nil {
			if b.board[b.curRow][b.curCol+1].posType == Wall {
				return true
			}
			b.curCol++
			return false
		} else {
			right := p.right
			if b.board[right.row][right.col].posType == Wall {
				return true
			}
			b.curCol = right.col
			return false
		}
	case Down:
		if p.down == nil {
			if b.board[b.curRow+1][b.curCol].posType == Wall {
				return true
			}
			b.curRow++
			return false
		} else {
			down := p.down
			if b.board[down.row][down.col].posType == Wall {
				return true
			}
			b.curRow = down.row
			return false
		}
	case Up:
		if p.up == nil {
			if b.board[b.curRow-1][b.curCol].posType == Wall {
				return true
			}
			b.curRow--
			return false
		} else {
			up := p.up
			if b.board[up.row][up.col].posType == Wall {
				return true
			}
			b.curRow = up.row
			return false
		}
	default:
		panic("unhandled case")
	}
}

// Return whether is blocked
func (b *Board) move2() bool {
	switch b.curDirection.direction {
	case Left:
		switch b.cubeType {
		case 1:
			if isWall, exists := b.exists(-1, 0, 0); exists {
				if isWall {
					return true
				}
				b.shift(-1, 0, 0)
				return false
			}
		case 2:
			if isWall, exists := b.exists(1, 0, 0); exists {
				if isWall {
					return true
				}
				b.shift(1, 0, 0)
				return false
			}
		case 3:
			if isWall, exists := b.exists(0, 0, 1); exists {
				if isWall {
					return true
				}
				b.shift(0, 0, 1)
				return false
			}
		case 4:
			if isWall, exists := b.exists(-1, 0, 0); exists {
				if isWall {
					return true
				}
				b.shift(-1, 0, 0)
				return false
			}
		case 5:
			if isWall, exists := b.exists(-1, 0, 0); exists {
				if isWall {
					return true
				}
				b.shift(-1, 0, 0)
				return false
			}
		case 6:
			if isWall, exists := b.exists(0, 0, -1); exists {
				if isWall {
					return true
				}
				b.shift(0, 0, -1)
				return false
			}
		}
	case Right:
		switch b.cubeType {
		case 1:
			if isWall, exists := b.exists(1, 0, 0); exists {
				if isWall {
					return true
				}
				b.shift(1, 0, 0)
				return false
			}
		case 2:
			if isWall, exists := b.exists(-1, 0, 0); exists {
				if isWall {
					return true
				}
				b.shift(-1, 0, 0)
				return false
			}
		case 3:
			if isWall, exists := b.exists(0, 0, -1); exists {
				if isWall {
					return true
				}
				b.shift(0, 0, -1)
				return false
			}
		case 4:
			if isWall, exists := b.exists(1, 0, 0); exists {
				if isWall {
					return true
				}
				b.shift(1, 0, 0)
				return false
			}
		case 5:
			if isWall, exists := b.exists(1, 0, 0); exists {
				if isWall {
					return true
				}
				b.shift(1, 0, 0)
				return false
			}
		case 6:
			if isWall, exists := b.exists(0, 0, 1); exists {
				if isWall {
					return true
				}
				b.shift(0, 0, 1)
				return false
			}
		}
	case Up:
		switch b.cubeType {
		case 1:
			if isWall, exists := b.exists(0, 0, 1); exists {
				if isWall {
					return true
				}
				b.shift(0, 0, 1)
				return false
			}
		case 2:
			if isWall, exists := b.exists(0, -1, 0); exists {
				if isWall {
					return true
				}
				b.shift(0, -1, 0)
				return false
			}
		case 3:
			if isWall, exists := b.exists(0, -1, 0); exists {
				if isWall {
					return true
				}
				b.shift(0, -1, 0)
				return false
			}
		case 4:
			if isWall, exists := b.exists(0, -1, 0); exists {
				if isWall {
					return true
				}
				b.shift(0, -1, 0)
				return false
			}
		case 5:
			if isWall, exists := b.exists(0, 0, -1); exists {
				if isWall {
					return true
				}
				b.shift(0, 0, -1)
				return false
			}
		case 6:
			if isWall, exists := b.exists(0, -1, 0); exists {
				if isWall {
					return true
				}
				b.shift(0, -1, 0)
				return false
			}
		}
	case Down:
		switch b.cubeType {
		case 1:
			if isWall, exists := b.exists(0, 0, -1); exists {
				if isWall {
					return true
				}
				b.shift(0, 0, -1)
				return false
			}
		case 2:
			if isWall, exists := b.exists(0, 1, 0); exists {
				if isWall {
					return true
				}
				b.shift(0, 1, 0)
				return false
			}
		case 3:
			if isWall, exists := b.exists(0, 1, 0); exists {
				if isWall {
					return true
				}
				b.shift(0, 1, 0)
				return false
			}
		case 4:
			if isWall, exists := b.exists(0, 1, 0); exists {
				if isWall {
					return true
				}
				b.shift(0, 1, 0)
				return false
			}
		case 5:
			if isWall, exists := b.exists(0, 0, 1); exists {
				if isWall {
					return true
				}
				b.shift(0, 0, 1)
				return false
			}
		case 6:
			if isWall, exists := b.exists(0, 1, 0); exists {
				if isWall {
					return true
				}
				b.shift(0, 1, 0)
				return false
			}
		}
	}

	return b.moveFace()
}

func (b *Board) moveFace() bool {
	x, y, z, direction, next := b.getCoordNextFace()

	if isWall, _ := b.isExist(x, y, z); isWall {
		return true
	}

	b.x = x
	b.y = y
	b.z = z
	b.curDirection.direction = direction
	b.cubeType = next
	return false
}

func (b *Board) getCoordNextFace() (int, int, int, DirectionType, int) {
	x := b.x
	y := b.y
	z := b.z
	end := b.cubeSize + 1
	switch b.cubeType {
	case 1:
		switch b.curDirection.direction {
		case Left:
			return 0, 1, z, Down, 3
		case Right:
			return end, 1, z, Down, 6
		case Up:
			return x, 1, end, Down, 2
		case Down:
			return x, 1, 0, Down, 4
		}
	case 2:
		switch b.curDirection.direction {
		case Left:
			return end, y, end - 1, Left, 6
		case Right:
			return 0, y, end - 1, Right, 3
		case Up:
			return x, 0, end - 1, Down, 1
		case Down:
			return x, end, end - 1, Up, 5
		}
	case 3:
		switch b.curDirection.direction {
		case Left:
			return 1, y, end, Left, 2
		case Right:
			return 1, y, 0, Right, 4
		case Up:
			return 1, 0, z, Right, 1
		case Down:
			return 1, end, z, Right, 5
		}
	case 4:
		switch b.curDirection.direction {
		case Left:
			return 0, y, 1, Left, 3
		case Right:
			return end, y, 1, Right, 6
		case Up:
			return x, 0, 1, Up, 1
		case Down:
			return x, end, 1, Down, 5
		}
	case 5:
		switch b.curDirection.direction {
		case Left:
			return 0, end - 1, z, Up, 3
		case Right:
			return end, end - 1, z, Up, 6
		case Up:
			return x, end - 1, 0, Up, 4
		case Down:
			return x, end - 1, end, Up, 2
		}
	case 6:
		switch b.curDirection.direction {
		case Left:
			return end - 1, y, 0, Left, 4
		case Right:
			return end - 1, y, end, Right, 2
		case Up:
			return end - 1, 0, z, Left, 1
		case Down:
			return end - 1, end, z, Left, 5
		}
	}

	panic("unhandled")
}

func (b *Board) shift(shiftX, shiftY, shiftZ int) {
	b.x += shiftX
	b.y += shiftY
	b.z += shiftZ
}

func (b *Board) exists(shiftX, shiftY, shiftZ int) (bool, bool) {
	x := b.x + shiftX
	y := b.y + shiftY
	z := b.z + shiftZ

	return b.isExist(x, y, z)
}

func (b *Board) isExist(x, y, z int) (bool, bool) {
	if x < 0 || x > b.cubeSize+1 || y < 0 || y > b.cubeSize+1 || z < 0 || z > b.cubeSize+1 {
		return false, false
	}

	if b.cube[x][y][z] == nil {
		return false, false
	}

	return b.cube[x][y][z].posType == Wall, true
}

func (b *Board) print() {
	fmt.Println("----------Board----------")
	for row := 0; row < b.maxRow; row++ {
		for col := 0; col < b.maxCol; col++ {
			if row == b.curRow && col == b.curCol {
				switch b.curDirection.direction {
				case Left:
					fmt.Print("<")
				case Right:
					fmt.Print(">")
				case Down:
					fmt.Print("v")
				case Up:
					fmt.Print("^")
				}
				continue
			}

			switch b.board[row][col].posType {
			case Empty:
				fmt.Print(" ")
			case Open:
				fmt.Print(".")
			case Wall:
				fmt.Print("#")
			}
		}
		fmt.Println()
	}
}

func (b *Board) printFaces() {
	fmt.Println("----------Board----------")
	for row := 0; row < b.maxRow; row++ {
		for col := 0; col < b.maxCol; col++ {
			switch b.board[row][col].posType {
			case Empty:
				fmt.Print(" ")
			default:
				fmt.Printf("%d", b.board[row][col].face)
			}
		}
		fmt.Println()
	}
}

func (b *Board) printCube() {
	fmt.Println("----------Face1----------")
	for z := b.cubeSize; z >= 1; z-- {
		for x := 1; x <= b.cubeSize; x++ {
			fmt.Printf("%c", b.getUnit(x, 0, z))
		}
		fmt.Println()
	}
	fmt.Println("----------Face2----------")
	z := b.cubeSize + 1
	for y := 1; y <= b.cubeSize; y++ {
		for x := b.cubeSize; x >= 1; x-- {
			fmt.Printf("%c", b.getUnit(x, y, z))
		}
		fmt.Println()
	}
	fmt.Println("----------Face3----------")
	x := 0
	for y := 1; y <= b.cubeSize; y++ {
		for z := b.cubeSize; z >= 1; z-- {
			fmt.Printf("%c", b.getUnit(x, y, z))
		}
		fmt.Println()
	}
	fmt.Println("----------Face4----------")
	z = 0
	for y := 1; y <= b.cubeSize; y++ {
		for x := 1; x <= b.cubeSize; x++ {
			fmt.Printf("%c", b.getUnit(x, y, z))
		}
		fmt.Println()
	}
	fmt.Println("----------Face5----------")
	y := b.cubeSize + 1
	for z := 1; z <= b.cubeSize; z++ {
		for x := 1; x <= b.cubeSize; x++ {
			fmt.Printf("%c", b.getUnit(x, y, z))
		}
		fmt.Println()
	}
	fmt.Println("----------Face6----------")
	x = b.cubeSize + 1
	for z := 1; z <= b.cubeSize; z++ {
		for y := b.cubeSize; y >= 1; y-- {
			fmt.Printf("%c", b.getUnit(x, y, z))
		}
		fmt.Println()
	}
}

func (b *Board) printCubeFace() {
	if b.cubeType == 1 {
		fmt.Println("----------Face1----------")
		for z := b.cubeSize; z >= 1; z-- {
			for x := 1; x <= b.cubeSize; x++ {
				fmt.Printf("%c", b.getUnit(x, 0, z))
			}
			fmt.Println()
		}
	} else if b.cubeType == 2 {
		fmt.Println("----------Face2----------")
		z := b.cubeSize + 1
		for y := 1; y <= b.cubeSize; y++ {
			for x := b.cubeSize; x >= 1; x-- {
				fmt.Printf("%c", b.getUnit(x, y, z))
			}
			fmt.Println()
		}
	} else if b.cubeType == 3 {
		fmt.Println("----------Face3----------")
		x := 0
		for y := 1; y <= b.cubeSize; y++ {
			for z := b.cubeSize; z >= 1; z-- {
				fmt.Printf("%c", b.getUnit(x, y, z))
			}
			fmt.Println()
		}
	} else if b.cubeType == 4 {
		fmt.Println("----------Face4----------")
		z := 0
		for y := 1; y <= b.cubeSize; y++ {
			for x := 1; x <= b.cubeSize; x++ {
				fmt.Printf("%c", b.getUnit(x, y, z))
			}
			fmt.Println()
		}
	} else if b.cubeType == 5 {
		fmt.Println("----------Face5----------")
		y := b.cubeSize + 1
		for z := 1; z <= b.cubeSize; z++ {
			for x := 1; x <= b.cubeSize; x++ {
				fmt.Printf("%c", b.getUnit(x, y, z))
			}
			fmt.Println()
		}
	} else if b.cubeType == 6 {
		fmt.Println("----------Face6----------")
		x := b.cubeSize + 1
		for z := 1; z <= b.cubeSize; z++ {
			for y := b.cubeSize; y >= 1; y-- {
				fmt.Printf("%c", b.getUnit(x, y, z))
			}
			fmt.Println()
		}
	}
}

func (b *Board) getUnit(x, y, z int) rune {
	if x == b.x && y == b.y && z == b.z {
		switch b.curDirection.direction {
		case Up:
			return 'U'
		case Down:
			return 'D'
		case Left:
			return 'L'
		case Right:
			return 'R'
		}
	}

	switch b.cube[x][y][z].posType {
	case Empty:
		return ' '
	case Open:
		return '.'
	case Wall:
		return '#'
	}
	panic("get unit")
}

func toBoard(lines []string) Board {
	maxRow := len(lines)
	maxCol := 0
	for _, line := range lines {
		if len(line) > maxCol {
			maxCol = len(line)
		}
	}

	board := make([][]Position, maxRow)
	for row := 0; row < maxRow; row++ {
		board[row] = make([]Position, maxCol)
		for col := 0; col < maxCol; col++ {
			board[row][col] = Position{row: row, col: col}
		}
	}

	for row, line := range lines {
		runes := []rune(line)
		for col, rune := range runes {
			switch rune {
			case '.':
				board[row][col].posType = Open
			case '#':
				board[row][col].posType = Wall
			}
		}
	}

	curCol := 0
	curRow := 0
	for col := 0; col < maxCol; col++ {
		if board[0][col].posType == Open {
			curCol = col
			break
		}
	}

	for row, positions := range board {
		startingCol := -1
		for col := 0; col < maxCol; col++ {
			if positions[col].posType != Empty {
				startingCol = col
				break
			}
		}

		endingCol := -1
		for col := maxCol - 1; col >= 0; col-- {
			if positions[col].posType != Empty {
				endingCol = col
				break
			}
		}

		board[row][startingCol].left = &board[row][endingCol]
		board[row][endingCol].right = &board[row][startingCol]
	}

	for col := 0; col < maxCol; col++ {
		startingRow := -1
		for row := 0; row < maxRow; row++ {
			if board[row][col].posType != Empty {
				startingRow = row
				break
			}
		}

		endingRow := -1
		for row := maxRow - 1; row >= 0; row-- {
			if board[row][col].posType != Empty {
				endingRow = row
				break
			}
		}

		board[startingRow][col].up = &board[endingRow][col]
		board[endingRow][col].down = &board[startingRow][col]
	}

	return Board{
		board:  board,
		maxRow: maxRow,
		maxCol: maxCol,
		curRow: curRow,
		curCol: curCol,
		curDirection: &Direction{
			direction: Right,
		},
	}
}

func toBoard2(lines []string, cubeSize int, hints func(cubeSize int, faces map[int]Face, cube [][][]*Position, board [][]Position)) Board {
	maxRow := len(lines)
	maxCol := 0
	for _, line := range lines {
		if len(line) > maxCol {
			maxCol = len(line)
		}
	}

	board := make([][]Position, maxRow)
	for row := 0; row < maxRow; row++ {
		board[row] = make([]Position, maxCol)
		for col := 0; col < maxCol; col++ {
			board[row][col] = Position{row: row, col: col}
		}
	}

	for row, line := range lines {
		runes := []rune(line)
		for col, rune := range runes {
			switch rune {
			case '.':
				board[row][col].posType = Open
			case '#':
				board[row][col].posType = Wall
			}
		}
	}

	curCol := 0
	curRow := 0
	for col := 0; col < maxCol; col++ {
		if board[0][col].posType == Open {
			curCol = col
			break
		}
	}

	visited := make([][]bool, maxRow)
	for row := 0; row < maxRow; row++ {
		visited[row] = make([]bool, maxCol)
	}

	face := 1
	faces := make(map[int]Face)
	for row := 0; row < maxRow; row++ {
		for col := 0; col < maxCol; col++ {
			if visited[row][col] {
				continue
			}

			if board[row][col].posType != Empty {
				faces[face] = Face{row: row, col: col, value: face}
				for i := 0; i < cubeSize; i++ {
					for j := 0; j < cubeSize; j++ {
						board[row+i][col+j].face = face
						visited[row+i][col+j] = true
					}
				}
				face++
			}
		}
	}

	b := Board{
		board:  board,
		maxRow: maxRow,
		maxCol: maxCol,
		curRow: curRow,
		curCol: curCol,
		curDirection: &Direction{
			direction: Right,
		},
		hints:    hints,
		faces:    faces,
		cubeSize: cubeSize,
		cubeType: 1,
		x:        1,
		y:        0,
		z:        cubeSize,
	}

	for i, face := range faces {
		if v := b.getLeftFace(face.row, face.col-1); v != 0 {
			face.left = v
			faces[i] = face
		}
		if v := b.getRightFace(face.row, face.col+cubeSize); v != 0 {
			face.right = v
			faces[i] = face
		}
		if v := b.getUpFace(face.row-1, face.col); v != 0 {
			face.up = v
			faces[i] = face
		}
		if v := b.getDownFace(face.row+cubeSize, face.col); v != 0 {
			face.down = v
			faces[i] = face
		}
	}

	fmt.Printf("%#v\n", faces)

	cube := make([][][]*Position, cubeSize+2)
	for i := 0; i < cubeSize+2; i++ {
		cube[i] = make([][]*Position, cubeSize+2)
		for j := 0; j < cubeSize+2; j++ {
			cube[i][j] = make([]*Position, cubeSize+2)
		}
	}

	hints(cubeSize, faces, cube, board)
	b.cube = cube

	return b
}

type Face struct {
	// Top left
	row   int
	col   int
	value int

	left  int
	right int
	up    int
	down  int
}

type Position struct {
	row     int
	col     int
	posType Type
	left    *Position
	right   *Position
	up      *Position
	down    *Position
	face    int

	fromLeft  DirectionType
	fromRight DirectionType
	fromUp    DirectionType
	fromDown  DirectionType
}

type Type int

const (
	Empty Type = iota
	Open
	Wall
)

type Step struct {
	isMove bool

	move int
	turn DirectionType
}

func toSteps(s string) []Step {
	var steps []Step
	runes := []rune(s)
	for i := 0; i < len(runes); i++ {
		if runes[i] == 'R' {
			steps = append(steps, Step{turn: Right})
		} else if runes[i] == 'L' {
			steps = append(steps, Step{turn: Left})
		}

		n := toDigit(runes[i])
		i++
		for {
			if i == len(s) {
				steps = append(steps, Step{isMove: true, move: n})
				break
			}
			if runes[i] == 'L' || runes[i] == 'R' {
				i--
				steps = append(steps, Step{isMove: true, move: n})
				break
			}
			n = n*10 + toDigit(runes[i])
			i++
		}
	}
	return steps
}

func toDigit(r rune) int {
	s := string(r)
	i, _ := strconv.Atoi(s)
	return i
}

func fs2(input io.Reader, cubeSize int, hints func(cubeSize int, faces map[int]Face, cube [][][]*Position, board [][]Position)) (int, error) {
	scanner := bufio.NewScanner(input)
	var lines []string
	for scanner.Scan() {
		line := scanner.Text()
		lines = append(lines, line)
	}

	board := toBoard2(lines[:len(lines)-2], cubeSize, hints)
	//board.printFaces()
	//board.printCube()
	//board.print()

	steps := toSteps(lines[len(lines)-1])
	for _, step := range steps {
		if step.isMove {
			for i := 0; i < step.move; i++ {
				isBlocked := board.move2()
				if isBlocked {
					break
				}
				//board.printCubeFace()
			}
		} else {
			if step.turn == Left {
				board.left()
			} else {
				board.right()
			}
			//board.printCubeFace()
		}
	}

	pos := board.cube[board.x][board.y][board.z]

	res := (pos.row+1)*1000 + (pos.col+1)*4 + board.getDirectionValue()
	return res, nil
}

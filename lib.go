package aoc

import (
	"bufio"
	"fmt"
	"io"
	"math"
	"strconv"
	"strings"
)

/*
1. Push
2. go get github.com/teivah/advent-of-code@main
3. Run IntelliJ "Go Mod Tidy"
*/

// ---------- Parsing inputs ----------

func ReaderToString(input io.Reader) string {
	scanner := bufio.NewScanner(input)
	scanner.Scan()
	return scanner.Text()
}

type Delimiter struct {
	Ind []int
	s   string
	del string
}

func NewDelimiter(s, del string) Delimiter {
	return Delimiter{
		Ind: IndexAll(s, del),
		s:   s,
		del: del,
	}
}

func (d Delimiter) GetStrings() []string {
	if len(d.Ind) == 0 {
		if d.s == "" {
			return nil
		}

		return []string{d.s}
	}

	var res []string
	for i := 0; i <= len(d.Ind); i++ {
		res = append(res, d.GetString(i))
	}

	return res
}

func (d Delimiter) GetInts(s []string) []int {
	return StringsToInts(s)
}

func (d Delimiter) GetString(i int) string {
	if i == 0 {
		return d.s[:d.Ind[0]]
	}

	if i == len(d.Ind) {
		return d.s[d.Ind[len(d.Ind)-1]+len(d.del):]
	}

	return d.s[d.Ind[i-1]+len(d.del) : d.Ind[i]]
}

func (d Delimiter) GetInt(i int) int {
	return StringToInt(d.GetString(i))
}

func (d Delimiter) TryGetInt(i int) (int, bool) {
	return TryStringToInt(d.GetString(i))
}

func (d Delimiter) IsInt(i int) bool {
	_, err := strconv.Atoi(d.GetString(i))
	return err == nil
}

func IndexAll(s string, search string) []int {
	i := 0
	var res []int
	for i < len(s) {
		index := strings.Index(s[i:], search)
		if index == -1 {
			return res
		}
		res = append(res, index+i)
		i += index + len(search)
	}
	return res
}

func ReaderToStrings(input io.Reader) []string {
	scanner := bufio.NewScanner(input)
	var strings []string
	for scanner.Scan() {
		strings = append(strings, scanner.Text())
	}
	return strings
}

func ReaderToInts(input io.Reader) []int {
	scanner := bufio.NewScanner(input)
	var res []int
	for scanner.Scan() {
		res = append(res, StringToInt(scanner.Text()))
	}
	return res
}

// ---------- String ----------

func StringPermutations(idx int, runes []rune) []string {
	if idx == len(runes) {
		return []string{string(runes)}
	}

	var res []string
	for i := idx; i < len(runes); i++ {
		runes[i], runes[idx] = runes[idx], runes[i]
		res = append(res, StringPermutations(idx+1, runes)...)
		runes[i], runes[idx] = runes[idx], runes[i]
	}
	return res
}

func RuneToInt(r rune) int {
	return int(r - '0')
}

func IntToRune(i int) rune {
	s := strconv.Itoa(i)
	return []rune(s)[0]
}

// ---------- Math ----------

type Maxer struct {
	max int
}

func NewMaxer() *Maxer {
	return &Maxer{
		max: math.MinInt,
	}
}

func (m *Maxer) Add(values ...int) {
	for _, v := range values {
		m.max = Max(m.max, v)
	}
}

func (m *Maxer) Get() int {
	return m.max
}

type Miner struct {
	min int
}

func NewMiner() *Miner {
	return &Miner{
		min: math.MaxInt,
	}
}

func (m *Miner) Add(values ...int) {
	for _, v := range values {
		m.min = Min(m.min, v)
	}
}

func (m *Miner) Get() int {
	return m.min
}

func Min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func Max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

func MaxInts(values []int) int {
	max := math.MinInt
	for _, v := range values {
		max = Max(max, v)
	}
	return max
}

func MinInts(values []int) int {
	min := math.MaxInt
	for _, v := range values {
		min = Min(min, v)
	}
	return min
}

func Mod(d, m int) int {
	res := d % m
	if (res < 0 && m > 0) || (res > 0 && m < 0) {
		return res + m
	}
	return res
}

func Abs(a int) int {
	if a >= 0 {
		return a
	}
	return -a
}

func ManhattanDistance(row, col int) int {
	return Abs(row) + Abs(col)
}

func GreatestCommonDivisor(a, b int) int {
	for b != 0 {
		a, b = b, a%b
	}
	return a
}

func LeastCommonMultiple(numbers []int) int {
	lcm := numbers[0]
	for _, number := range numbers[1:] {
		lcm = lcm * number / GreatestCommonDivisor(lcm, number)
	}
	return lcm
}

// ---------- Conversions ----------

func StringToInt(s string) int {
	i, err := strconv.Atoi(s)
	if err != nil {
		panic(err)
	}
	return i
}

func TryStringToInt(s string) (int, bool) {
	i, err := strconv.Atoi(s)
	if err != nil {
		return 0, false
	}
	return i, true
}


func StringsToInts(s []string) []int {
	res := make([]int, len(s))
	for i, v := range s {
		res[i] = StringToInt(v)
	}
	return res
}

func StringGroups(lines []string) [][]string {
	i := 0
	var res [][]string
	var row []string
	for {
		row = append(row, lines[i])
		i++
		if i >= len(lines) {
			res = append(res, row)
			break
		}
		for ; i < len(lines); i++ {
			if lines[i] == "" {
				break
			} else {
				row = append(row, lines[i])
			}
		}
		res = append(res, row)
		row = nil
		i++
		if i >= len(lines) {
			break
		}
	}
	return res
}

// Collections

func CopyInts(s []int) []int {
	res := make([]int, len(s))
	for i, v := range s {
		res[i] = v
	}
	return res
}

// Directions

type Direction int

const (
	Up Direction = iota
	Down
	Left
	Right

	UpLeft
	UpRight
	DownLeft
	DownRight
)

func (d Direction) Opposite() Direction {
	switch d {
	case Up:
		return Down
	case Down:
		return Up
	case Left:
		return Right
	case Right:
		return Left
	}
	panic("not handled")
}

func (d Direction) Turn(turn Direction) Direction {
	if turn == Up || turn == Down {
		return turn
	}

	switch d {
	case Up:
		return turn
	case Down:
		switch turn {
		case Left:
			return Right
		case Right:
			return Left
		}
	case Left:
		switch turn {
		case Left:
			return Down
		case Right:
			return Up
		}
	case Right:
		switch turn {
		case Left:
			return Up
		case Right:
			return Down
		}
	}

	panic("not handled")
}

type Position struct {
	Row int
	Col int
}

func (p Position) String() string {
	return fmt.Sprintf("row=%d, col=%d", p.Row, p.Col)
}

func (p Position) Manhattan(p2 Position) int {
	return Abs(p.Row-p2.Row) + Abs(p.Col-p2.Col)
}

func (p Position) ManhattanZero() int {
	return p.Manhattan(Position{})
}

func (p Position) Delta(row, col int) Position {
	return Position{
		Row: p.Row + row,
		Col: p.Col + col,
	}
}

func (p Position) Move(direction Direction, moves int) Position {
	switch direction {
	case Up:
		return p.Delta(-moves, 0)
	case Down:
		return p.Delta(moves, 0)
	case Left:
		return p.Delta(0, -moves)
	case Right:
		return p.Delta(0, moves)
	case UpLeft:
		return p.Delta(-moves, -moves)
	case UpRight:
		return p.Delta(-moves, moves)
	case DownLeft:
		return p.Delta(moves, -moves)
	case DownRight:
		return p.Delta(moves, moves)
	}

	panic ("not handled")
}
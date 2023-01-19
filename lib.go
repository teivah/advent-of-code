package lib

import (
	"bufio"
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
}

func NewDelimiter(s, del string) Delimiter {
	return Delimiter{
		Ind: IndexAll(s, del),
		s:   s,
	}
}

func (d Delimiter) GetString(i int) string {
	if i == 0 {
		return d.s[:d.Ind[0]]
	}

	if i == len(d.Ind) {
		return d.s[d.Ind[len(d.Ind)-1]+1:]
	}

	return d.s[d.Ind[i-1]+1 : d.Ind[i]]
}

func (d Delimiter) GetInt(i int) int {
	return StringToInt(d.GetString(i))
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

// ---------- Math ----------

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

// ---------- Conversions ----------

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

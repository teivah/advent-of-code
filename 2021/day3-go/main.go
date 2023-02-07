package main

import (
	"io"
	"strconv"

	aoc "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	lines := aoc.ReaderToStrings(input)
	gr := gammaRate(lines)
	er := epsilonRate(lines)

	a, err := strconv.ParseInt(gr, 2, 64)
	if err != nil {
		panic(err)
	}

	b, err := strconv.ParseInt(er, 2, 64)
	if err != nil {
		panic(err)
	}

	return int(a) * int(b)
}

func gammaRate(lines []string) string {
	res := ""
	for i := 0; i < len(lines[0]); i++ {
		zero := 0
		one := 0
		for j := 0; j < len(lines); j++ {
			if lines[j][i] == '0' {
				zero++
			} else {
				one++
			}
		}
		if zero > one {
			res += "0"
		} else {
			res += "1"
		}
	}
	return res
}

func epsilonRate(lines []string) string {
	res := ""
	for i := 0; i < len(lines[0]); i++ {
		zero := 0
		one := 0
		for j := 0; j < len(lines); j++ {
			if lines[j][i] == '0' {
				zero++
			} else {
				one++
			}
		}
		if zero < one {
			res += "0"
		} else {
			res += "1"
		}
	}
	return res
}

func fs2(input io.Reader) int {
	lines := aoc.ReaderToStrings(input)
	or := oxygenRating(lines)
	cr := co2Rating(lines)

	a, err := strconv.ParseInt(or, 2, 64)
	if err != nil {
		panic(err)
	}

	b, err := strconv.ParseInt(cr, 2, 64)
	if err != nil {
		panic(err)
	}

	return int(a) * int(b)
}

func oxygenRating(lines []string) string {
	oxygen := make(map[int]struct{}, len(lines))
	for i := 0; i < len(lines); i++ {
		oxygen[i] = struct{}{}
	}

	for i := 0; i < len(lines[0]); i++ {
		if len(oxygen) == 1 {
			return lines[getFirstKey(oxygen)]
		}

		zero := 0
		one := 0
		for k := range oxygen {
			line := lines[k]
			if line[i] == '0' {
				zero++
			} else {
				one++
			}
		}

		for j := 0; j < len(lines); j++ {
			if lines[j][i] == '0' {
				if one > zero {
					delete(oxygen, j)
				} else if zero > one {
				} else {
					delete(oxygen, j)
				}
			} else {
				if one > zero {
				} else if zero > one {
					delete(oxygen, j)
				} else {
				}
			}
		}
	}

	return lines[getFirstKey(oxygen)]
}

func co2Rating(lines []string) string {
	co2 := make(map[int]struct{}, len(lines))
	for i := 0; i < len(lines); i++ {
		co2[i] = struct{}{}
	}

	for i := 0; i < len(lines[0]); i++ {
		if len(co2) == 1 {
			return lines[getFirstKey(co2)]
		}

		zero := 0
		one := 0
		for k := range co2 {
			line := lines[k]
			if line[i] == '0' {
				zero++
			} else {
				one++
			}
		}

		for j := 0; j < len(lines); j++ {
			if lines[j][i] == '0' {
				if one > zero {
				} else if zero > one {
					delete(co2, j)
				} else {
				}
			} else {
				if one > zero {
					delete(co2, j)
				} else if zero > one {
				} else {
					delete(co2, j)
				}
			}
		}
	}

	return ""
}

func getFirstKey(m map[int]struct{}) int {
	for k := range m {
		return k
	}
	return -1
}

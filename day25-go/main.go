package main

import (
	"bufio"
	"fmt"
	"io"
	"math"
)

func fs1(input io.Reader) (string, error) {
	scanner := bufio.NewScanner(input)
	sum := 0
	for scanner.Scan() {
		line := scanner.Text()
		v := toInt(line)
		sum += v
	}

	fmt.Println(sum)

	snafu, runes := toSnafu(0, sum, 0, nil)
	fmt.Println(snafu)

	return toString(runes), nil
}

func toInt(s string) int {
	exp := 0
	sum := 0
	for i := len(s) - 1; i >= 0; i-- {
		v := 0
		switch s[i] {
		case '1':
			v = 1
		case '2':
			v = 2
		case '0':
			v = 0
		case '-':
			v = -1
		case '=':
			v = -2
		}
		sum += v * int(math.Pow(5, float64(exp)))
		exp++
	}
	return sum
}

/*
4890
2
12
62
*/
func toSnafu(pow, target, current int, s []rune) (int, []rune) {
	n := 2*int(math.Pow(5, float64(pow))) + current
	s = append(s, '2')
	if target == n {
		return target, s
	}
	if n < target {
		found, runes := toSnafu(pow+1, target, n, s)
		if found == target {
			return target, runes
		}
		found -= 2 * int(math.Pow(5, float64(pow)))
		for i := 1; i >= -2; i-- {
			n := i*int(math.Pow(5, float64(pow))) + found
			if n == target {
				runes[pow] = toRune(i)
				return target, runes
			}
			if n > target {
				continue
			}
			runes[pow] = toRune(i + 1)
			n = (i+1)*int(math.Pow(5, float64(pow))) + found
			return n, runes
		}
		runes[pow] = '='
		n = (-2)*int(math.Pow(5, float64(pow))) + found
		return n, runes
	}

	for i := 1; i >= -2; i-- {
		n := i*int(math.Pow(5, float64(pow))) + current
		s[len(s)-1] = toRune(i)
		if target == n {
			return target, s
		}
		if n > target {
			continue
		} else {
			s[len(s)-1] = toRune(i + 1)
			n = (i+1)*int(math.Pow(5, float64(pow))) + current
			return n, s
		}
	}

	return -1, nil
}

func toString(runes []rune) string {
	s := ""
	for i := len(runes) - 1; i >= 0; i-- {
		s += fmt.Sprintf("%c", runes[i])
	}
	return s
}

func toRune(i int) rune {
	switch i {
	case 2:
		return '2'
	case 1:
		return '1'
	case 0:
		return '0'
	case -1:
		return '-'
	case -2:
		return '='
	default:
		panic("to rune")
	}
}

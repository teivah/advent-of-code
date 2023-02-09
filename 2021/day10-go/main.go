package main

import (
	"bufio"
	"fmt"
	"io"
	"sort"
)

func fs1(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	sum := 0
	for scanner.Scan() {
		line := scanner.Text()
		sum += score(line)
	}

	return sum
}

func score(s string) int {
	var q []Sign
	par := 0
	squareBracket := 0
	bracket := 0
	comp := 0
	for i := 0; i < len(s); i++ {
		sign := Sign(s[i])
		switch sign {
		case parOpen:
			par++
		case parClose:
			par--
		case squareBracketOpen:
			squareBracket++
		case squareBracketClose:
			squareBracket--
		case bracketOpen:
			bracket++
		case bracketClose:
			bracket--
		case compOpen:
			comp++
		case compClose:
			comp--
		}

		if !isClose(sign) {
			q = append(q, sign)
			continue
		}

		h := q[len(q)-1]
		if isOpposite(h, sign) {
			q = q[:len(q)-1]
			continue
		}

		return points(sign)
	}
	return 0
}

type Sign rune

func points(sign Sign) int {
	switch sign {
	case parClose:
		return 3
	case squareBracketClose:
		return 57
	case bracketClose:
		return 1197
	case compClose:
		return 25137
	}
	panic(string(sign))
}

func points2(sign Sign) int {
	switch sign {
	case parOpen:
		return 1
	case squareBracketOpen:
		return 2
	case bracketOpen:
		return 3
	case compOpen:
		return 4
	}
	panic(string(sign))
}

func isClose(sign Sign) bool {
	return sign == parClose || sign == squareBracketClose || sign == bracketClose || sign == compClose
}

func isOpposite(sign1, sign2 Sign) bool {
	switch sign1 {
	case parOpen:
		return sign2 == parClose
	case squareBracketOpen:
		return sign2 == squareBracketClose
	case bracketOpen:
		return sign2 == bracketClose
	case compOpen:
		return sign2 == compClose
	}
	panic(fmt.Sprintf("%s, %s", string(sign1), string(sign2)))
}

const (
	parOpen            Sign = '('
	parClose           Sign = ')'
	squareBracketOpen  Sign = '['
	squareBracketClose Sign = ']'
	bracketOpen        Sign = '{'
	bracketClose       Sign = '}'
	compOpen           Sign = '<'
	compClose          Sign = '>'
)

func fs2(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	var valid []string
	for scanner.Scan() {
		line := scanner.Text()
		if score(line) == 0 {
			valid = append(valid, line)
		}
	}

	var scores []int
	for _, s := range valid {
		scores = append(scores, missing(s))
	}
	sort.Ints(scores)

	return scores[len(scores)/2]
}

func missing(s string) int {
	var q []Sign
	par := 0
	squareBracket := 0
	bracket := 0
	comp := 0
	for i := 0; i < len(s); i++ {
		sign := Sign(s[i])
		switch sign {
		case parOpen:
			par++
		case parClose:
			par--
		case squareBracketOpen:
			squareBracket++
		case squareBracketClose:
			squareBracket--
		case bracketOpen:
			bracket++
		case bracketClose:
			bracket--
		case compOpen:
			comp++
		case compClose:
			comp--
		}

		if !isClose(sign) {
			q = append(q, sign)
			continue
		}

		h := q[len(q)-1]
		if isOpposite(h, sign) {
			q = q[:len(q)-1]
			continue
		}

		return points(sign)
	}

	sum := 0
	res := ""
	for len(q) != 0 {
		h := q[len(q)-1]
		res += string(h)
		q = q[:len(q)-1]
		sum = sum*5 + points2(h)
	}
	return sum
}

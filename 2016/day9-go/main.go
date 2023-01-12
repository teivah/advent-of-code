package main

import (
	"bufio"
	"io"
	"strconv"
)

func fs1(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	scanner.Scan()
	s := scanner.Text()

	sum := 0
	for i := 0; i < len(s); i++ {
		if s[i] == '(' {
			i++
			j := i + 1
			for {
				if !isDigit(s[j]) {
					break
				}
				j++
			}
			nchars, err := strconv.Atoi(s[i:j])
			if err != nil {
				panic(err)
			}

			i = j + 1
			j = i + 1
			for {
				if !isDigit(s[j]) {
					break
				}
				j++
			}
			repeat, err := strconv.Atoi(s[i:j])
			if err != nil {
				panic(err)
			}

			sum += nchars * repeat

			// Char after marker
			i = j + nchars
		} else {
			sum++
		}
	}

	return sum
}

func isDigit(u uint8) bool {
	return u >= '0' && u <= '9'
}

func fs2(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	scanner.Scan()
	s := scanner.Text()

	weights := make([]int, len(s))
	for i := 0; i < len(s); i++ {
		weights[i] = 1
	}

	sum := 0
	for i := 0; i < len(s); i++ {
		c := s[i]
		current := weights[i]

		if c != '(' {
			sum += weights[i]
		} else {
			i++
			j := i + 1
			for {
				if !isDigit(s[j]) {
					break
				}
				j++
			}
			nchars, err := strconv.Atoi(s[i:j])
			if err != nil {
				panic(err)
			}

			i = j + 1
			j = i + 1
			for {
				if !isDigit(s[j]) {
					break
				}
				j++
			}
			repeat, err := strconv.Atoi(s[i:j])
			if err != nil {
				panic(err)
			}

			// Char after marker
			i = j + 1

			for j := i; j < i+nchars && j < len(s); j++ {
				weights[j] = current * repeat
			}
			i--
		}
	}

	return sum
}

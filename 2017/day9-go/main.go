package main

import (
	"bufio"
	"io"
)

func fs1(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	scanner.Scan()
	s := scanner.Text()

	return groups(s)
}

func groups(s string) int {
	open := 1
	sum := 0
	for i := 0; i < len(s); i++ {
		r := rune(s[i])

		if r == '<' {
			j := i + 1
			for ; j < len(s); j++ {
				r := rune(s[j])
				if r == '!' {
					j++
				} else if r == '>' {
					break
				}
			}
			i = j
		} else if r == '{' {
			sum += open
			open++
		} else if r == '}' {
			open--
		} else if r == ',' {
		} else {
			panic(string(r))
		}
	}
	return sum
}

func fs2(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	scanner.Scan()
	s := scanner.Text()

	return groups2(s)
}

func groups2(s string) int {
	open := 1
	sum := 0
	for i := 0; i < len(s); i++ {
		r := rune(s[i])

		if r == '<' {
			j := i + 1
			for ; j < len(s); j++ {
				r := rune(s[j])
				if r == '!' {
					j++
				} else if r == '>' {
					break
				} else {
					sum++
				}
			}
			i = j
		} else if r == '{' {
			open++
		} else if r == '}' {
			open--
		} else if r == ',' {
		} else {
			panic(string(r))
		}
	}
	return sum
}

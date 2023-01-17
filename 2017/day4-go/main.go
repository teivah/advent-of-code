package main

import (
	"bufio"
	"github.com/teivah/advent-of-code/lib"
	"io"
	"strings"
)

func fs1(input io.Reader) (int, error) {
	scanner := bufio.NewScanner(input)
	sum := 0
	for scanner.Scan() {
		if isValid(scanner.Text()) {
			sum++
		}
	}

	lib.IndexAll("a", "b")

	return sum, nil
}

func isValid(s string) bool {
	words := strings.Split(s, " ")
	set := make(map[string]struct{})
	for _, word := range words {
		if _, exists := set[word]; exists {
			return false
		}
		set[word] = struct{}{}
	}
	return true
}

func fs2(input io.Reader) (int, error) {
	scanner := bufio.NewScanner(input)
	sum := 0
	for scanner.Scan() {
		if isValid2(scanner.Text()) {
			sum++
		}
	}

	return sum, nil
}

func isValid2(s string) bool {
	words := strings.Split(s, " ")
	for i := 0; i < len(words); i++ {
		a := words[i]
		for j := 0; j < i; j++ {
			b := words[j]
			if isAnagram(a, b) {
				return false
			}
		}
	}
	return true
}

func isAnagram(a, b string) bool {

}

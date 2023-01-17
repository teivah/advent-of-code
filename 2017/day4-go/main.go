package main

import (
	"bufio"
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
	if len(a) != len(b) {
		return false
	}

	lettersA := make(map[rune]int)
	for i := 0; i < len(a); i++ {
		r := rune(a[i])
		lettersA[r]++
	}

	lettersB := make(map[rune]int)
	for i := 0; i < len(b); i++ {
		r := rune(b[i])
		lettersB[r]++
	}

	if len(lettersA) != len(lettersB) {
		return false
	}

	for k, v := range lettersA {
		v2, exists := lettersB[k]
		if !exists {
			return false
		}
		if v != v2 {
			return false
		}
	}

	return true
}

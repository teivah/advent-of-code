package main

import (
	"bufio"
	"fmt"
	"io"
	"strings"
)

func fs1(s string, n int) int {
	for i := 0; i < n; i++ {
		s = apply(s)
	}
	return len(s)
}

func apply(s string) string {
	runes := []rune(s)

	previous := runes[0]
	cur := 1
	sb := strings.Builder{}

	for i := 1; i < len(runes); i++ {
		rune := runes[i]
		if rune == previous {
			cur++
		} else {
			sb.WriteString(fmt.Sprintf("%d%c", cur, previous))
			previous = rune
			cur = 1
		}
	}

	sb.WriteString(fmt.Sprintf("%d%c", cur, previous))

	return sb.String()
}

func fs2(input io.Reader) (int, error) {
	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
		line := scanner.Text()
		_ = line
	}

	return 0, nil
}

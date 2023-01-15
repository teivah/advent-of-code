package main

import (
	"bufio"
	"io"
	"strings"
)

func fs(a string, disk int) (string, error) {
	for {
		a = random(a)
		if len(a) >= disk {
			break
		}
	}

	return checksum(a[:disk]), nil
}

func random(a string) string {
	sb := strings.Builder{}
	sb.Grow(len(a))
	for i := len(a) - 1; i >= 0; i-- {
		c := a[i]
		if c == '0' {
			sb.WriteByte('1')
		} else {
			sb.WriteByte('0')
		}
	}

	return a + "0" + sb.String()
}

func checksum(a string) string {
	for {
		sb := strings.Builder{}
		sb.Grow(len(a) / 2)
		for i := 0; i < len(a)-1; i += 2 {
			c1 := a[i]
			c2 := a[i+1]
			if c1 == c2 {
				sb.WriteByte('1')
			} else {
				sb.WriteByte('0')
			}
		}

		a = sb.String()
		if len(a)%2 != 0 {
			return a
		}
	}
}

func fs2(input io.Reader) (int, error) {
	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
		line := scanner.Text()
		_ = line
	}

	return 42, nil
}

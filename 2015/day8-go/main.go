package main

import (
	"bufio"
	"io"
	"strconv"
)

func fs1(input io.Reader) (int, error) {
	scanner := bufio.NewScanner(input)
	sum := 0
	for scanner.Scan() {
		line := scanner.Text()
		s, err := strconv.Unquote(line)
		if err != nil {
			return 0, err
		}
		sum += len(line) - len(s)
	}

	return sum, nil
}

func fs2(input io.Reader) (int, error) {
	scanner := bufio.NewScanner(input)
	sum := 0
	for scanner.Scan() {
		line := scanner.Text()
		sum += len(strconv.Quote(line)) - len(line)
	}

	return sum, nil
}

package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

const totalStars = 450

func main() {
	if err := run(); err != nil {
		panic(err)
	}
}

func run() error {
	f, err := os.Open("README.md")
	if err != nil {
		return err
	}

	stats := map[string]int{
		"Go":      0,
		"Rust":    0,
		"Haskell": 0,
		"Python":  0,
	}
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		line := scanner.Text()

		for language := range stats {
			if strings.Contains(line, fmt.Sprintf("[%s]", language)) {
				stats[language] += 2
			} else if strings.Contains(line, fmt.Sprintf("[%s ", language)) {
				stats[language] += 1
			}
		}
	}

	for language, count := range stats {
		fmt.Printf("%s: %.2f%%\n", language, float64(count)/float64(totalStars)*100)
	}
	return nil
}

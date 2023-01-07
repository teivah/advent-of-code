package main

import (
	"bufio"
	"io"
	"strconv"
	"strings"
)

func fs1(input io.Reader, after int) (int, error) {
	scanner := bufio.NewScanner(input)
	var reindeers []Reindeer
	for scanner.Scan() {
		s := scanner.Text()
		idx := indexAll(s, " ")

		speed, err := strconv.Atoi(s[idx[2]+1 : idx[3]])
		if err != nil {
			return 0, err
		}

		seconds, err := strconv.Atoi(s[idx[5]+1 : idx[6]])
		if err != nil {
			return 0, err
		}

		rest, err := strconv.Atoi(s[idx[12]+1 : idx[13]])
		if err != nil {
			return 0, err
		}

		reindeers = append(
			reindeers, Reindeer{
				speed:   speed,
				seconds: seconds,
				rest:    rest,
			},
		)
	}

	rests := make([]int, len(reindeers))
	distances := make([]int, len(reindeers))
	fly := make([]int, len(reindeers))
	for i, reindeer := range reindeers {
		fly[i] = reindeer.seconds
	}
	for i := 0; i < after; i++ {
		for j, reindeer := range reindeers {
			if rests[j] != 0 {
				rests[j]--
				if rests[j] == 0 {
					fly[j] = reindeer.seconds
				}
				continue
			}

			distances[j] += reindeer.speed
			fly[j]--
			if fly[j] == 0 {
				rests[j] = reindeer.rest
			}
		}
	}

	best := 0
	for _, distance := range distances {
		best = max(best, distance)
	}

	return best, nil
}

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

type Reindeer struct {
	speed   int
	seconds int
	rest    int
}

func fs2(input io.Reader, after int) (int, error) {
	scanner := bufio.NewScanner(input)
	var reindeers []Reindeer
	for scanner.Scan() {
		s := scanner.Text()
		idx := indexAll(s, " ")

		speed, err := strconv.Atoi(s[idx[2]+1 : idx[3]])
		if err != nil {
			return 0, err
		}

		seconds, err := strconv.Atoi(s[idx[5]+1 : idx[6]])
		if err != nil {
			return 0, err
		}

		rest, err := strconv.Atoi(s[idx[12]+1 : idx[13]])
		if err != nil {
			return 0, err
		}

		reindeers = append(
			reindeers, Reindeer{
				speed:   speed,
				seconds: seconds,
				rest:    rest,
			},
		)
	}

	rests := make([]int, len(reindeers))
	points := make([]int, len(reindeers))
	distances := make([]int, len(reindeers))
	fly := make([]int, len(reindeers))
	for i, reindeer := range reindeers {
		fly[i] = reindeer.seconds
	}
	for i := 0; i < after; i++ {
		for j, reindeer := range reindeers {
			if rests[j] != 0 {
				rests[j]--
				if rests[j] == 0 {
					fly[j] = reindeer.seconds
				}
				continue
			}

			distances[j] += reindeer.speed
			fly[j]--
			if fly[j] == 0 {
				rests[j] = reindeer.rest
			}
		}

		best := -1
		for j := range reindeers {
			best = max(best, distances[j])
		}
		for j := range reindeers {
			if distances[j] == best {
				points[j]++
			}
		}
	}

	best := 0
	for _, point := range points {
		best = max(best, point)
	}

	return best, nil
}

func indexAll(s string, search string) []int {
	i := 0
	var res []int
	for i < len(s) {
		index := strings.Index(s[i:], search)
		if index == -1 {
			return res
		}
		res = append(res, index+i)
		i += index + len(search)
	}
	return res
}

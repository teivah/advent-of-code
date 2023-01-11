package main

import (
	"bufio"
	"io"
	"sort"
	"strconv"
)

func fs1(input io.Reader) (int, error) {
	scanner := bufio.NewScanner(input)
	var triangles [][]int
	for scanner.Scan() {
		triangle, err := numbers(scanner.Text())
		if err != nil {
			return 0, err
		}
		triangles = append(triangles, triangle)
	}

	sum := 0
	for _, triangle := range triangles {
		if isValid(triangle) {
			sum++
		}
	}

	return sum, nil
}

func isValid(triangle []int) bool {
	sort.Ints(triangle)
	return triangle[0]+triangle[1] > triangle[2]
}

func numbers(s string) ([]int, error) {
	var res []int
	for i := 0; i < len(s); i++ {
		if isNumber(s[i]) {
			j := i + 1
			for ; j < len(s); j++ {
				if !isNumber(s[j]) {
					break
				}
			}
			v := s[i:j]
			n, err := strconv.Atoi(v)
			if err != nil {
				return nil, err
			}
			res = append(res, n)
			i = j
		}
	}
	return res, nil
}

func isNumber(u uint8) bool {
	return u >= '0' && u <= '9'
}

func fs2(input io.Reader) (int, error) {
	scanner := bufio.NewScanner(input)
	var triangles [][]int
	for scanner.Scan() {
		triangle, err := numbers(scanner.Text())
		if err != nil {
			return 0, err
		}
		triangles = append(triangles, triangle)
	}

	sum := 0
	for col := 0; col < 3; col++ {
		for row := 0; row < len(triangles); row += 3 {
			if isValid([]int{triangles[row][col], triangles[row+1][col], triangles[row+2][col]}) {
				sum++
			}
		}
	}

	return sum, nil
}

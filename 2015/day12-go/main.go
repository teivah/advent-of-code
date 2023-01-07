package main

import (
	"io"
	"strconv"
)

func fs1(input io.Reader) (int, error) {
	b, err := io.ReadAll(input)
	if err != nil {
		return 0, err
	}
	s := string(b)
	runes := []rune(s)

	sum := 0
	for i := 0; i < len(runes); i++ {
		c := runes[i]
		if c == '-' || (c >= '0' && c <= '9') {
			j := i + 1
			for ; j < len(runes); j++ {
				c := runes[j]
				if c >= '0' && c <= '9' {
					continue
				}
				break
			}
			n := s[i:j]
			v, err := strconv.Atoi(n)
			if err != nil {
				return 0, err
			}
			sum += v
			i = j
		}
	}

	return sum, nil
}

func fs2(input io.Reader) (int, error) {
	b, err := io.ReadAll(input)
	if err != nil {
		return 0, err
	}
	return nored(string(b), 0), nil
}

func nored(s string, i int) int {
	if i > len(s) {
		return 0
	}

	r := s[i]
	if r == '{' {
		v, _ := object(s, i+1)
		return v
	}
	if r == '[' {
		v, _ := array(s, i+1)
		return v
	}

	panic(i)
}

func object(s string, i int) (int, int) {
	r := s[i]
	if r == '}' {
		return 0, i
	}

	red := false
	sum := 0
	for i < len(s) {
		r := s[i]

		if r == '"' {
			j := i + 1
			for ; j < len(s); j++ {
				c := s[j]
				if c == '"' {
					break
				}
			}
			i = j + 2 // remove :

			r = s[i]
			if r == '{' {
				v, pos := object(s, i+1)
				i = pos + 1
				sum += v
			} else if r == '[' {
				v, pos := array(s, i+1)
				i = pos + 1
				sum += v
			} else if r == '-' || (r >= '0' && r <= '9') {
				j := i + 1
				for ; j < len(s); j++ {
					c := s[j]
					if c >= '0' && c <= '9' {
						continue
					}
					break
				}
				n := s[i:j]
				v, err := strconv.Atoi(n)
				if err != nil {
					panic(err)
				}
				sum += v
				i = j
			} else if r == '"' {
				j := i + 1
				for ; j < len(s); j++ {
					c := s[j]
					if c == '"' {
						break
					}
				}

				v := s[i+1 : j]
				if v == "red" {
					red = true
				}

				i = j + 1
			}
		} else if r == ',' {
			i++
		} else if r == '}' {
			break
		}
	}

	if red {
		return 0, i
	}
	return sum, i
}

func array(s string, i int) (int, int) {
	r := s[i]
	if r == ']' {
		return 0, i
	}

	sum := 0
	for i < len(s) {
		r := s[i]

		if r == ']' {
			return sum, i
		} else if r == '-' || (r >= '0' && r <= '9') {
			j := i + 1
			for ; j < len(s); j++ {
				c := s[j]
				if c >= '0' && c <= '9' {
					continue
				}
				break
			}
			n := s[i:j]
			v, err := strconv.Atoi(n)
			if err != nil {
				panic(err)
			}
			sum += v
			i = j
		} else if r == '"' {
			j := i + 1
			for ; j < len(s); j++ {
				c := s[j]
				if c == '"' {
					break
				}
			}
			i = j + 1
		} else if r == '{' {
			v, pos := object(s, i+1)
			i = pos + 1
			sum += v
		} else if r == '[' {
			v, pos := array(s, i+1)
			i = pos + 1
			sum += v
		} else if r == ',' {
			i++
		}
	}

	return sum, i
}

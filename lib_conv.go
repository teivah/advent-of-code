package aoc

import "strconv"

// StringToInt is an optimistic conversion from a string to an int.
func StringToInt(s string) int {
	i, err := strconv.Atoi(s)
	if err != nil {
		panic(err)
	}
	return i
}

// TryStringToInt tries to convert a string into an int.
func TryStringToInt(s string) (int, bool) {
	i, err := strconv.Atoi(s)
	if err != nil {
		return 0, false
	}
	return i, true
}

// StringsToInts is an optimistic conversion from a slice of strings to a slice
// of ints.
func StringsToInts(s []string) []int {
	res := make([]int, len(s))
	for i, v := range s {
		res[i] = StringToInt(v)
	}
	return res
}

// RuneToInt converts a rune into an int.
func RuneToInt(r rune) int {
	return int(r - '0')
}

// IntToRune converts an int into a rune.
func IntToRune(i int) rune {
	s := strconv.Itoa(i)
	return []rune(s)[0]
}

// IsRuneDecimal checks whether a rune is a decimal number.
func IsRuneDecimal(r rune) bool {
	return r >= '0' && r <= '9'
}

package aoc

// Mod returns the modulo.
func Mod(d, m int) int {
	res := d % m
	if (res < 0 && m > 0) || (res > 0 && m < 0) {
		return res + m
	}
	return res
}

// Abs returns the absolute value.
func Abs(a int) int {
	if a >= 0 {
		return a
	}
	return -a
}

// ManhattanDistance returns the manhattan distance.
func ManhattanDistance(row, col int) int {
	return Abs(row) + Abs(col)
}

// GreatestCommonDivisor returns the greatest common divisor of two numbers.
func GreatestCommonDivisor(a, b int) int {
	for b != 0 {
		a, b = b, a%b
	}
	return a
}

// LeastCommonMultiple returns the least common multiple from a list of numbers.
func LeastCommonMultiple(numbers []int) int {
	lcm := numbers[0]
	for _, number := range numbers[1:] {
		lcm = lcm * number / GreatestCommonDivisor(lcm, number)
	}
	return lcm
}

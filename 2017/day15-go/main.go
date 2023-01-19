package main

/*
next = (previous * factor) % 2147483647

factor a = 16807
factor b = 48271
*/
const (
	factorA   = 16807
	factorB   = 48271
	remainder = 2147483647
)

func fs1(repeat, a, b int) int {
	sum := 0
	for i := 0; i < repeat; i++ {
		a = (a * factorA) % remainder
		b = (b * factorB) % remainder
		if a<<48 == b<<48 {
			sum++
		}
	}
	return sum
}

/*
a % 4 == 0
b % 8 == 0
*/
func fs2(repeat, a, b int) int {
	sum := 0
	for i := 0; i < repeat; i++ {
		for {
			a = (a * factorA) % remainder
			if a%4 == 0 {
				break
			}
		}

		for {
			b = (b * factorB) % remainder
			if b%8 == 0 {
				break
			}
		}

		if a<<48 == b<<48 {
			sum++
		}
	}
	return sum
}

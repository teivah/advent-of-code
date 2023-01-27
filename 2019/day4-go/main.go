package main

func fs1(from, to int) int {
	sum := 0
	for i := from; i <= to; i++ {
		digits := toDigits(i)
		if twoAdjacentDigits(digits) && neverDecrease(digits) {
			sum++
		}
	}

	return sum
}

func toDigits(i int) []int {
	var v []int
	for i != 0 {
		v = append(v, i%10)
		i /= 10
	}

	res := make([]int, len(v))
	for idx := 0; idx < len(v); idx++ {
		res[idx] = v[len(v)-idx-1]
	}
	return res
}

func twoAdjacentDigits(digits []int) bool {
	for i := 1; i < len(digits); i++ {
		if digits[i] == digits[i-1] {
			return true
		}
	}
	return false
}

func twoExactDigits(digits []int) bool {
	for i := 1; i < len(digits); i++ {
		if digits[i] == digits[i-1] {
			if i > 1 {
				if digits[i-2] == digits[i] {
					continue
				}
			}
			if i < len(digits)-1 {
				if digits[i] == digits[i+1] {
					continue
				}
			}
			return true
		}
	}
	return false
}

func neverDecrease(digits []int) bool {
	for i := 1; i < len(digits); i++ {
		if digits[i] < digits[i-1] {
			return false
		}
	}
	return true
}

func fs2(from, to int) int {
	sum := 0
	for i := from; i <= to; i++ {
		digits := toDigits(i)
		if twoExactDigits(digits) && neverDecrease(digits) {
			sum++
		}
	}

	return sum
}

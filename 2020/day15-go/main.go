package main

func fs(nth int, input []int) int {
	lastTime := make(map[int]int)
	lastNumber := 0
	for i := 0; i < len(input); i++ {
		v := input[i]
		lastNumber = v

		if i != len(input)-1 {
			lastTime[v] = i
		}
	}

	for i := len(input); i < nth; i++ {
		v := 0
		previousRound, exists := lastTime[lastNumber]
		if !exists {

		} else {
			v = i - previousRound - 1
		}

		lastTime[lastNumber] = i - 1
		lastNumber = v
	}

	return lastNumber
}

package main

func fs1(n, repeat int) int {
	s := []int{0}
	pos := 0
	for step := 0; step < repeat; step++ {
		for i := 0; i < n; i++ {
			pos = (pos + 1) % len(s)
		}
		if pos == len(s)-1 {
			s = append(s, step+1)
		} else {
			s = insert(s, pos+1, step+1)
		}
		pos = (pos + 1) % len(s)
	}
	return s[pos+1]
}

func insert(s []int, i, v int) []int {
	res := make([]int, 0, len(s)+1)
	for j := 0; j < len(s); j++ {
		if j == i {
			res = append(res, v)
		}
		res = append(res, s[j])
	}
	return res
}

func fs2(n, repeat int) int {
	pos := 1
	res := -1
	for step := 0; step < repeat; step++ {
		if step == 0 {
			pos = 1
		} else {
			pos = (pos+n)%(step+1) + 1
		}
		if pos == 1 {
			res = step + 1
		}
	}

	return res
}

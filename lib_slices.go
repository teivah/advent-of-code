package aoc

import (
	"golang.org/x/exp/constraints"
)

func SliceCopy[T any](in []T) []T {
	res := make([]T, len(in))
	copy(res, in)
	return res
}

func SliceWithoutIndex[T any](in []T, idx int) []T {
	if len(in) == 0 {
		return nil
	}
	e := SliceCopy(in)
	if idx < 0 || idx >= len(in) {
		panic(idx)
	}
	if idx == 0 {
		return e[1:]
	} else if idx == len(e)-1 {
		return e[0 : len(e)-1]
	} else {
		return append(e[0:idx], e[idx+1:]...)
	}
}

func IsSliceSortedFunc[T constraints.Ordered](in []T, f func(a T, b T) bool) bool {
	if len(in) <= 1 {
		return true
	}
	prev := in[0]
	for i := 1; i < len(in); i++ {
		cur := in[i]
		if !f(prev, cur) {
			return false
		}
		prev = cur
	}
	return true
}

func IsSliceMonotonicallyIncreasing[T constraints.Ordered](in []T) bool {
	return IsSliceSortedFunc(in, func(a T, b T) bool {
		return b >= a
	})
}

func IsSliceMonotonicallyDecreasing[T constraints.Ordered](in []T) bool {
	return IsSliceSortedFunc(in, func(a T, b T) bool {
		return b <= a
	})
}

func IsSliceStrictlyIncreasing[T constraints.Ordered](in []T) bool {
	return IsSliceSortedFunc(in, func(a T, b T) bool {
		return b > a
	})
}

func IsSliceStrictlyDecreasing[T constraints.Ordered](in []T) bool {
	return IsSliceSortedFunc(in, func(a T, b T) bool {
		return b < a
	})
}

func CountSliceOccurrence[T comparable](in []T) map[T]int {
	res := make(map[T]int)
	for _, v := range in {
		res[v]++
	}
	return res
}

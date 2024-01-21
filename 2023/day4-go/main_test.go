package main

import (
	"testing"
)

//go:nosplit
func funcWithNoSplit() int {
	return 1
}

func LoopWithNoSplit() int {
	sum := 0
	for i := 0; i < 1e6; i++ {
		sum += funcWithNoSplit()
		sum += funcWithNoSplit()
		sum += funcWithNoSplit()
		sum += funcWithNoSplit()
		sum += funcWithNoSplit()
	}
	return sum
}

func funcWithSplit() int {
	return 1
}

func LoopWithSplit() int {
	sum := 0
	for i := 0; i < 1e6; i++ {
		sum += funcWithSplit()
		sum += funcWithSplit()
		sum += funcWithSplit()
		sum += funcWithSplit()
		sum += funcWithSplit()
	}
	return sum
}

var global int

func BenchmarkNoSplit(b *testing.B) {
	for i := 0; i < b.N; i++ {
		_ = LoopWithNoSplit()
	}
}

func BenchmarkSplit(b *testing.B) {
	for i := 0; i < b.N; i++ {
		_ = LoopWithSplit()
	}
}

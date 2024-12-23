package main

import (
	"io"
	"math"

	"github.com/teivah/go-aoc"
)

func fs1(input io.Reader, count int) int {
	secrets := aoc.ReaderToInts(input)
	res := 0
	for _, secret := range secrets {
		for i := 0; i < count; i++ {
			secret = it(secret)
		}
		res += secret
	}
	return res
}

func it(s int) int {
	v := s * 64
	s = mix(s, v)
	s = prune(s)
	f := int(math.Floor(float64(s) / 32))
	s = mix(s, f)
	s = prune(s)
	v = s * 2048
	s = mix(s, v)
	s = prune(s)
	return s
}

func mix(s, v int) int {
	return v ^ s
}

func prune(s int) int {
	return aoc.Mod(s, 16777216)
}

func fs2(input io.Reader, count int) int {
	secrets := aoc.ReaderToInts(input)
	var seqs [][]int
	for _, secret := range secrets {
		seqs = append(seqs, seq(secret, count))
	}

	allSequences := make(map[Sequence]bool)
	for _, s := range seqs {
		v := getAllSequences(s)
		for _, n := range v {
			allSequences[n] = true
		}
	}

	res := 0
	for x := range allSequences {
		n := 0
		for _, s := range seqs {
			n += find(s, x)
		}
		res = max(res, n)
	}
	return res
}

func find(seq []int, x Sequence) int {
	for i := 4; i < len(seq); i++ {
		found := Sequence{
			price1: seq[i-3] - seq[i-4],
			price2: seq[i-2] - seq[i-3],
			price3: seq[i-1] - seq[i-2],
			price4: seq[i] - seq[i-1],
		}
		if found == x {
			return seq[i]
		}
	}
	return 0
}

func getAllSequences(seq []int) []Sequence {
	var diffs []int
	for i := 1; i < len(seq); i++ {
		diffs = append(diffs, seq[i]-seq[i-1])
	}
	var sequences []Sequence
	for i := 3; i < len(diffs); i++ {
		sequences = append(sequences, Sequence{
			price1: diffs[i-3],
			price2: diffs[i-2],
			price3: diffs[i-1],
			price4: diffs[i],
		})
	}
	return sequences
}

type Sequence struct {
	price1 int
	price2 int
	price3 int
	price4 int
}

func seq(secret int, count int) []int {
	var res []int
	res = append(res, secret%10)
	for i := 0; i < count; i++ {
		secret = it(secret)
		res = append(res, secret%10)
	}
	return res
}

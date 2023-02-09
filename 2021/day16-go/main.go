package main

import (
	"fmt"
	"io"
	"strings"

	aoc "github.com/teivah/advent-of-code"
)

func fs1(input io.Reader) int {
	s := aoc.ReaderToString(input)
	b := toBinary(s)

	version, _ := packets(b, 0)

	return version
}

func packets(b string, i int) (int, int) {
	packetVersion := toInt(b[i : i+3])
	i += 3
	typeID := toInt(b[i : i+3])
	i += 3
	sumVersion := 0
	if typeID == 4 {
		sb := strings.Builder{}
		for {
			sb.WriteString(b[i+1 : i+5])
			if b[i] == '0' {
				i += 5
				break
			}
			i += 5
		}
		v := toInt(sb.String())
		_ = v
	} else {
		lengthTypeID := b[i]
		i++
		if lengthTypeID == '0' {
			totalLengthInBits := toInt(b[i : i+15])
			i += 15
			target := i + totalLengthInBits
			for i < target {
				v, j := packets(b, i)
				sumVersion += v
				i = j
			}
		} else {
			numberOfSubPackets := toInt(b[i : i+11])
			i += 11
			for packet := 0; packet < numberOfSubPackets; packet++ {
				v, j := packets(b, i)
				sumVersion += v
				i = j
			}
		}
	}
	return packetVersion + sumVersion, i
}

func parsePackets(b string, i int) (int, int) {
	sb := strings.Builder{}
	for {
		sb.WriteString(b[i+1 : i+5])
		if b[i] == '0' {
			i += 5
			break
		}
		i += 5
	}
	return toInt(sb.String()), i
}

func toInt(s string) int {
	v := 0
	for i := 0; i < len(s); i++ {
		if s[i] == '1' {
			v += 1 << (len(s) - i - 1)
		}
	}
	return v
}

func toBinary(s string) string {
	sb := strings.Builder{}
	for i := 0; i < len(s); i++ {
		r := s[i]
		if r >= 'A' {
			r = ':' + (r - 'A')
		}
		sb.WriteString(fmt.Sprintf("%4b", rune(r))[2:])
	}
	return sb.String()
}

func fs2(input io.Reader) int {
	s := aoc.ReaderToString(input)
	b := toBinary(s)

	version, _ := packets(b, 0)

	return version
}

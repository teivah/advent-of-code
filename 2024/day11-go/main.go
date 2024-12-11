package main

import (
	"fmt"
	"io"

	"github.com/teivah/go-aoc"
)

func fs(input io.Reader, count int) int {
	del := aoc.NewDelimiter(aoc.ReaderToString(input), " ")
	digits := del.GetInts()
	head := aoc.NewNode(digits[0])
	cur := head
	for i := 1; i < len(digits); i++ {
		cur.Next = aoc.NewNode(digits[i])
		cur = cur.Next
	}

	for i := 0; i < count; i++ {
		cur = head
		for cur != nil {
			s := fmt.Sprintf("%d", cur.Data)
			switch {
			case cur.Data == 0:
				cur.Data = 1
				cur = cur.Next
			case len(s)%2 == 0:
				a := aoc.StringToInt(s[:len(s)/2])
				b := aoc.StringToInt(s[len(s)/2:])
				cur.Data = a
				oldNext := cur.Next
				cur.Next = aoc.NewNode(b)
				cur.Next.Next = oldNext
				cur = oldNext
			default:
				cur.Data *= 2024
				cur = cur.Next
			}
		}
	}

	res := 0
	cur = head
	for cur != nil {
		res++
		cur = cur.Next
	}
	return res
}

func fs2(input io.Reader) int {
	_ = aoc.ReaderToStrings(input)
	return 42
}

package main

import (
	"bufio"
	"fmt"
	"io"
)

/*
handshake:
start with v=1
repeat 0 .. loop size:
	v *= subject number
	v %= 20201227

card: cardPK = sn(7)
door: doorPK = sn(Y)


*/
func fs1(cardPK, doorPK int) int {
	cardLoopSize := 0
	for ; ; cardLoopSize++ {
		v := subjectNumber(7, cardLoopSize)
		if v == cardPK {
			break
		}
	}
	fmt.Println(cardLoopSize)

	doorLoopSize := 0
	for ; ; doorLoopSize++ {
		v := subjectNumber(7, doorLoopSize)
		if v == doorPK {
			break
		}
	}

	return subjectNumber(doorPK, cardLoopSize)
}

func subjectNumber(sn, loop int) int {
	v := 1
	for i := 0; i < loop; i++ {
		v *= sn
		v %= 20201227
	}
	return v
}

func fs2(input io.Reader) int {
	scanner := bufio.NewScanner(input)
	for scanner.Scan() {
		line := scanner.Text()
		_ = line
	}

	return 42
}

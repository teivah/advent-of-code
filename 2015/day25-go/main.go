package main

import (
	"math/big"
)

func fs1(targetRow, targetCol int) string {
	var n *big.Int
	for row := 1; ; row++ {
		r := row
		for col := 1; ; col++ {
			if r == 1 && col == 1 {
				n = big.NewInt(20151125)
			} else {
				n = code(n)
			}

			if r == targetRow && col == targetCol {
				return n.String()
			}

			if col == row {
				break
			}
			r--
		}
	}
}

func code(a *big.Int) *big.Int {
	v := new(big.Int)
	a = v.Mul(a, big.NewInt(252533))
	v = new(big.Int)
	return v.Mod(a, big.NewInt(33554393))
}

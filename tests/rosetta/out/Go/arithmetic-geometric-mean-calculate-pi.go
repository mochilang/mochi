//go:build ignore

package main

import (
	"fmt"
)

// line 4
func abs(x float64) float64 {
	if x < 0.0 {
		return -x
	}
	return x
}

// line 9
func sqrtApprox(x float64) float64 {
	var guess float64 = x
	var i int = 0
	for {
		if !(i < 20) {
			break
		}
		guess = ((guess + (x / guess)) / 2.0)
		i = (i + 1)
	}
	return guess
}

// line 19
func agmPi() float64 {
	var a float64 = 1.0
	var g float64 = (1.0 / sqrtApprox(2.0))
	var sum float64 = 0.0
	var pow float64 = 2.0
	for {
		if !(abs((a - g)) > 0.000000000000001) {
			break
		}
		var t float64 = ((a + g) / 2.0)
		var u float64 = sqrtApprox((a * g))
		a = t
		g = u
		pow = (pow * 2.0)
		var diff float64 = ((a * a) - (g * g))
		sum = (sum + (diff * pow))
	}
	var pi float64 = (((4.0 * a) * a) / (1.0 - sum))
	return pi
}

// line 37
func main() {
	fmt.Println(fmt.Sprint(agmPi()))
}

func main() {
	main()
}

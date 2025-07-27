//go:build ignore
// +build ignore

package main

import (
	"fmt"
	"math"
)

func harmonic(n int) float64 {
	sum := 0.0
	for i := 1; i <= n; i++ {
		sum += 1.0 / float64(i)
	}
	return sum
}

func main() {
	n := 1000000
	gamma := harmonic(n) - math.Log(float64(n))
	fmt.Printf("%.6f\n", gamma)
}

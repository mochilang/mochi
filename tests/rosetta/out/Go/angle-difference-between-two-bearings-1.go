//go:build ignore

package main

import (
	"fmt"
)

// line 3
func angleDiff(b1 float64, b2 float64) float64 {
	var d float64 = (b2 - b1)
	if d < (float64(0) - 180.0) {
		return (d + 360.0)
	}
	if d > 180.0 {
		return (d - 360.0)
	}
	return d
}

func main() {
	var testCases [][]float64 = [][]float64{
		[]float64{20.0, 45.0},
		[]float64{(float64(0) - 45.0), 45.0},
		[]float64{(float64(0) - 85.0), 90.0},
		[]float64{(float64(0) - 95.0), 90.0},
		[]float64{(float64(0) - 45.0), 125.0},
		[]float64{(float64(0) - 45.0), 145.0},
		[]float64{29.4803, (float64(0) - 88.6381)},
		[]float64{(float64(0) - 78.3251), (float64(0) - 159.036)},
	}
	for _, tc := range testCases {
		fmt.Println(angleDiff(tc[0], tc[1]))
	}
}

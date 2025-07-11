//go:build ignore

package main

import (
	"fmt"
)

// line 3
func angleDiff(b1 float64, b2 float64) float64 {
	var diff float64 = (b2 - b1)
	return (((((diff % 360.0) + 360.0) + 180.0) % 360.0) - 180.0)
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
		[]float64{(float64(0) - 70099.74233810938), 29840.67437876723},
		[]float64{(float64(0) - 165313.6666297357), 33693.9894517456},
		[]float64{1174.8380510598456, (float64(0) - 154146.66490124757)},
		[]float64{60175.77306795546, 42213.07192354373},
	}
	for _, tc := range testCases {
		fmt.Println(angleDiff(tc[0], tc[1]))
	}
}

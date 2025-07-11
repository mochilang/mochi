//go:build ignore

package main

import (
	"fmt"
)

// line 4
func d2d(d float64) float64 {
	return (d % 360.0)
}

// line 5
func g2g(g float64) float64 {
	return (g % 400.0)
}

// line 6
func m2m(m float64) float64 {
	return (m % 6400.0)
}

// line 7
func r2r(r float64) float64 {
	return (r % (2.0 * 3.141592653589793))
}

// line 9
func d2g(d float64) float64 {
	return ((d2d(d) * 400.0) / 360.0)
}

// line 10
func d2m(d float64) float64 {
	return ((d2d(d) * 6400.0) / 360.0)
}

// line 11
func d2r(d float64) float64 {
	return ((d2d(d) * 3.141592653589793) / 180.0)
}

// line 13
func g2d(g float64) float64 {
	return ((g2g(g) * 360.0) / 400.0)
}

// line 14
func g2m(g float64) float64 {
	return ((g2g(g) * 6400.0) / 400.0)
}

// line 15
func g2r(g float64) float64 {
	return ((g2g(g) * 3.141592653589793) / 200.0)
}

// line 17
func m2d(m float64) float64 {
	return ((m2m(m) * 360.0) / 6400.0)
}

// line 18
func m2g(m float64) float64 {
	return ((m2m(m) * 400.0) / 6400.0)
}

// line 19
func m2r(m float64) float64 {
	return ((m2m(m) * 3.141592653589793) / 3200.0)
}

// line 21
func r2d(r float64) float64 {
	return ((r2r(r) * 180.0) / 3.141592653589793)
}

// line 22
func r2g(r float64) float64 {
	return ((r2r(r) * 200.0) / 3.141592653589793)
}

// line 23
func r2m(r float64) float64 {
	return ((r2r(r) * 3200.0) / 3.141592653589793)
}

// line 25
func main() {
	var angles []float64 = []float64{
		-2.0,
		-1.0,
		0.0,
		1.0,
		2.0,
		6.2831853,
		16.0,
		57.2957795,
		359.0,
		399.0,
		6399.0,
		1000000.0,
	}
	fmt.Println("degrees normalized_degs gradians mils radians")
	for _, a := range angles {
		fmt.Println(fmt.Sprint(a) + " " + fmt.Sprint(d2d(a)) + " " + fmt.Sprint(d2g(a)) + " " + fmt.Sprint(d2m(a)) + " " + fmt.Sprint(d2r(a)))
	}
	fmt.Println("\ngradians normalized_grds degrees mils radians")
	for _, a := range angles {
		fmt.Println(fmt.Sprint(a) + " " + fmt.Sprint(g2g(a)) + " " + fmt.Sprint(g2d(a)) + " " + fmt.Sprint(g2m(a)) + " " + fmt.Sprint(g2r(a)))
	}
	fmt.Println("\nmils normalized_mils degrees gradians radians")
	for _, a := range angles {
		fmt.Println(fmt.Sprint(a) + " " + fmt.Sprint(m2m(a)) + " " + fmt.Sprint(m2d(a)) + " " + fmt.Sprint(m2g(a)) + " " + fmt.Sprint(m2r(a)))
	}
	fmt.Println("\nradians normalized_rads degrees gradians mils")
	for _, a := range angles {
		fmt.Println(fmt.Sprint(a) + " " + fmt.Sprint(r2r(a)) + " " + fmt.Sprint(r2d(a)) + " " + fmt.Sprint(r2g(a)) + " " + fmt.Sprint(r2m(a)))
	}
}

func main() {
	main()
}

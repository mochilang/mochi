//go:build ignore

package main

import (
	"fmt"
)

// line 1
func validComb(a int, b int, c int, d int, e int, f int, g int) bool {
	var square1 int = (a + b)
	var square2 int = ((b + c) + d)
	var square3 int = ((d + e) + f)
	var square4 int = (f + g)
	return (((square1 == square2) && (square2 == square3)) && (square3 == square4))
}

// line 9
func isUnique(a int, b int, c int, d int, e int, f int, g int) bool {
	var nums []int = []int{
		a,
		b,
		c,
		d,
		e,
		f,
		g,
	}
	var i int = 0
	for {
		if !(i < len(nums)) {
			break
		}
		var j int = (i + 1)
		for {
			if !(j < len(nums)) {
				break
			}
			if nums[i] == nums[j] {
				return false
			}
			j = (j + 1)
		}
		i = (i + 1)
	}
	return true
}

// line 23
func getCombs(low int, high int, unique bool) map[string]any {
	var valid []any = []any{}
	var count int = 0
	for b := low; b < (high + 1); b++ {
		for c := low; c < (high + 1); c++ {
			for d := low; d < (high + 1); d++ {
				var s int = ((b + c) + d)
				for e := low; e < (high + 1); e++ {
					for f := low; f < (high + 1); f++ {
						var a int = (s - b)
						var g int = (s - f)
						if (a < low) || (a > high) {
							continue
						}
						if (g < low) || (g > high) {
							continue
						}
						if ((d + e) + f) != s {
							continue
						}
						if (f + g) != s {
							continue
						}
						if !unique || isUnique(a, b, c, d, e, f, g) {
							valid = append(valid, []int{
								a,
								b,
								c,
								d,
								e,
								f,
								g,
							})
							count = (count + 1)
						}
					}
				}
			}
		}
	}
	return map[string]any{"count": count, "list": valid}
}

func main() {
	var r1 map[string]any = getCombs(1, 7, true)
	fmt.Println(fmt.Sprint(r1["count"]) + " unique solutions in 1 to 7")
	fmt.Println(r1["list"])
	var r2 map[string]any = getCombs(3, 9, true)
	fmt.Println(fmt.Sprint(r2["count"]) + " unique solutions in 3 to 9")
	fmt.Println(r2["list"])
	var r3 map[string]any = getCombs(0, 9, false)
	fmt.Println(fmt.Sprint(r3["count"]) + " non-unique solutions in 0 to 9")
}

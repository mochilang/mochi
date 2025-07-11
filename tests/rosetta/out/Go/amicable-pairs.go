//go:build ignore

package main

import (
	"fmt"
)

// line 1
func pfacSum(i int) int {
	var sum int = 0
	var p int = 1
	for {
		if !(float64(p) <= (float64(i) / float64(2))) {
			break
		}
		if (i % p) == 0 {
			sum = (sum + p)
		}
		p = (p + 1)
	}
	return sum
}

// line 13
func pad(n int, width int) string {
	var s string = fmt.Sprint(n)
	for {
		if !(len(s) < width) {
			break
		}
		s = " " + s
	}
	return s
}

// line 21
func main() {
	var sums []int = []int{}
	var i int = 0
	for {
		if !(i < 20000) {
			break
		}
		sums = append(_convSlice[int, any](sums), 0)
		i = (i + 1)
	}
	i = 1
	for {
		if !(i < 20000) {
			break
		}
		sums[i] = pfacSum(i)
		i = (i + 1)
	}
	fmt.Println("The amicable pairs below 20,000 are:")
	var n int = 2
	for {
		if !(n < 19999) {
			break
		}
		var m int = sums[n]
		if ((m > n) && (m < 20000)) && (n == sums[m]) {
			fmt.Println("  " + pad(n, 5) + " and " + pad(m, 5))
		}
		n = (n + 1)
	}
}

func main() {
	main()
}

func _convSlice[T any, U any](s []T) []U {
	out := []U{}
	for _, v := range s {
		out = append(out, any(v).(U))
	}
	return out
}

//go:build ignore

// Generated by Mochi compiler v0.10.28 on 2006-01-02T15:04:05Z

package main

import (
	"fmt"
	"strings"
)

type v map[string]any

// line 5
func isPrime(n int) bool {
	if n < 2 {
		return false
	}
	if (n % 2) == 0 {
		return (n == 2)
	}
	if (n % 3) == 0 {
		return (n == 3)
	}
	d := 5
	for (d * d) <= n {
		if (n % d) == 0 {
			return false
		}
		d = (d + 2)
		if (n % d) == 0 {
			return false
		}
		d = (d + 4)
	}
	return true
}

// line 20
func bigTrim(a []int) []int {
	n := len(a)
	for (n > 1) && (a[(n-1)] == 0) {
		a = a[0:(n - 1)]
		n = (n - 1)
	}
	return a
}

// line 29
func bigFromInt(x int) []int {
	if x == 0 {
		return []int{0}
	}
	var digits []int = []int{}
	n := x
	for n > 0 {
		digits = append(_toAnySlice(digits), any((n % 10)))
		n = int(int(int((float64(n) / float64(10)))))
	}
	return digits
}

// line 40
func bigMulSmall(a []int, m int) []int {
	if m == 0 {
		return []int{0}
	}
	var res []int = []int{}
	carry := 0
	i := 0
	for i < len(a) {
		prod := ((a[i] * m) + carry)
		res = append(_toAnySlice(res), any((prod % 10)))
		carry = int(int(int((float64(prod) / float64(10)))))
		i = (i + 1)
	}
	for carry > 0 {
		res = append(_toAnySlice(res), any((carry % 10)))
		carry = int(int(int((float64(carry) / float64(10)))))
	}
	return bigTrim(res)
}

// line 58
func bigToString(a []int) string {
	s := ""
	i := (len(a) - 1)
	for i >= 0 {
		s = s + fmt.Sprint(any(a[i]))
		i = (i - 1)
	}
	return s
}

// line 69
func pow2(k int) int {
	r := 1
	i := 0
	for i < k {
		r = (r * 2)
		i = (i + 1)
	}
	return r
}

// line 79
func ccFactors(n int, m int) []int {
	p := ((6 * m) + 1)
	if !(isPrime(p)) {
		return []int{}
	}
	prod := bigFromInt(p)
	p = ((12 * m) + 1)
	if !(isPrime(p)) {
		return []int{}
	}
	prod = bigMulSmall(prod, p)
	i := 1
	for i <= (n - 2) {
		p = (((pow2(i) * 9) * m) + 1)
		if !(isPrime(p)) {
			return []int{}
		}
		prod = bigMulSmall(prod, p)
		i = (i + 1)
	}
	return prod
}

// line 96
func ccNumbers(start int, end int) {
	n := start
	for n <= end {
		m := 1
		if n > 4 {
			m = pow2((n - 4))
		}
		for {
			num := ccFactors(n, m)
			if len(num) > 0 {
				fmt.Println(strings.TrimSuffix(fmt.Sprintln(any("a("+fmt.Sprint(any(n))+") = "+bigToString(num))), "\n"))
				break
			}
			if n <= 4 {
				m = (m + 1)
			} else {
				m = (m + pow2((n - 4)))
			}
		}
		n = (n + 1)
	}
}

func main() {
	ccNumbers(3, 9)
}

func _toAnySlice[T any](s []T) []any {
	out := make([]any, len(s))
	for i, v := range s {
		out[i] = v
	}
	return out
}

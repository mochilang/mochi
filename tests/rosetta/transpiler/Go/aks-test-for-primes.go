//go:build ignore

// Generated by Mochi v0.10.39 on 2025-07-25 09:23:23 GMT+7
package main

import (
	"fmt"
)

func poly(p int) string {
	var s string = ""
	_ = s
	var coef int = 1
	_ = coef
	var i int = p
	_ = i
	if coef != 1 {
		s = (s + fmt.Sprint(coef))
	}
	for i > 0 {
		s = (s + "x")
		if i != 1 {
			s = ((s + "^") + fmt.Sprint(i))
		}
		coef = int(((coef * i) / ((p - i) + 1)))
		var d int = coef
		_ = d
		if ((p - (i - 1)) % 2) == 1 {
			d = (0 - d)
		}
		if d < 0 {
			s = ((s + " - ") + fmt.Sprint((0 - d)))
		} else {
			s = ((s + " + ") + fmt.Sprint(d))
		}
		i = (i - 1)
	}
	if s == "" {
		s = "1"
	}
	return s
}

func aks(n int) bool {
	if n < 2 {
		return false
	}
	var c int = n
	_ = c
	var i int = 1
	_ = i
	for i < n {
		if (c % n) != 0 {
			return false
		}
		c = int(((c * (n - i)) / (i + 1)))
		i = (i + 1)
	}
	return true
}

func mochiMain() {
	var p int = 0
	_ = p
	for p <= 7 {
		fmt.Println(((fmt.Sprint(p) + ":  ") + poly(p)))
		p = (p + 1)
	}
	var first bool = true
	_ = first
	p = 2
	var line string = ""
	_ = line
	for p < 50 {
		if aks(p) {
			if first {
				line = (line + fmt.Sprint(p))
				first = false
			} else {
				line = ((line + " ") + fmt.Sprint(p))
			}
		}
		p = (p + 1)
	}
	fmt.Println(line)
}

func main() {
	mochiMain()
}

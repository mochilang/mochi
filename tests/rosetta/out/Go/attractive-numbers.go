//go:build ignore

// Generated by Mochi compiler v0.10.28 on 2006-01-02T15:04:05Z

package main

import (
	"fmt"
	"strings"
)

type v map[string]any

// line 1
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

// line 15
func countPrimeFactors(n int) int {
	if n == 1 {
		return 0
	}
	if isPrime(n) {
		return 1
	}
	count := 0
	f := 2
	for {
		if (n % f) == 0 {
			count = (count + 1)
			n = int(int(int((float64(n) / float64(f)))))
			if n == 1 {
				return count
			}
			if isPrime(n) {
				f = n
			}
		} else if f >= 3 {
			f = (f + 2)
		} else {
			f = 3
		}
	}
	return count
}

// line 35
func pad4(n int) string {
	s := fmt.Sprint(any(n))
	for len(any(s)) < 4 {
		s = " " + s
	}
	return s
}

// line 43
func main() {
	max := 120
	fmt.Println(strings.TrimSuffix(fmt.Sprintln(any("The attractive numbers up to and including "+"120"+" are:")), "\n"))
	count := 0
	line := ""
	lineCount := 0
	i := 1
	for i <= max {
		c := countPrimeFactors(i)
		if isPrime(c) {
			line = line + pad4(i)
			count = (count + 1)
			lineCount = (lineCount + 1)
			if lineCount == 20 {
				fmt.Println(strings.TrimSuffix(fmt.Sprintln(any(line)), "\n"))
				line = ""
				lineCount = 0
			}
		}
		i = (i + 1)
	}
	if lineCount > 0 {
		fmt.Println(strings.TrimSuffix(fmt.Sprintln(any(line)), "\n"))
	}
}

func main() {
	main()
}

//go:build ignore

// Generated by Mochi compiler v0.10.28 on 2006-01-02T15:04:05Z

package main

import (
	"fmt"
	"strings"
)

type v map[string]any

// line 4
func primesUpTo(n int) []int {
	var sieve []bool = []bool{}
	i := 0
	for i <= n {
		sieve = append(_toAnySlice(sieve), any(true))
		i = (i + 1)
	}
	p := 2
	for (p * p) <= n {
		if sieve[p] {
			m := (p * p)
			for m <= n {
				sieve[m] = false
				m = (m + p)
			}
		}
		p = (p + 1)
	}
	var res []int = []int{}
	x := 2
	for x <= n {
		if sieve[x] {
			res = append(_toAnySlice(res), any(x))
		}
		x = (x + 1)
	}
	return res
}

// line 34
func longestSeq(dir string) {
	pd := 0
	var longSeqs [][]int = [][]int{[]int{2}}
	var currSeq []int = []int{2}
	i := 1
	for i < len(any(primes)) {
		d := (primes[i] - primes[(i-1)])
		if ((dir == "ascending") && (d <= pd)) || ((dir == "descending") && (d >= pd)) {
			if len(any(currSeq)) > len(any(longSeqs[0])) {
				longSeqs = [][]int{currSeq}
			} else if len(any(currSeq)) == len(any(longSeqs[0])) {
				longSeqs = append(_toAnySlice(longSeqs), any(currSeq))
			}
			currSeq = []int{primes[(i - 1)], primes[i]}
		} else {
			currSeq = append(_toAnySlice(currSeq), any(primes[i]))
		}
		pd = d
		i = (i + 1)
	}
	if len(any(currSeq)) > len(any(longSeqs[0])) {
		longSeqs = [][]int{currSeq}
	} else if len(any(currSeq)) == len(any(longSeqs[0])) {
		longSeqs = append(_toAnySlice(longSeqs), any(currSeq))
	}
	fmt.Println(strings.TrimSuffix(fmt.Sprintln(any("Longest run(s) of primes with "+dir+" differences is "+fmt.Sprint(any(len(any(longSeqs[0]))))+" :")), "\n"))
	for _, ls := range longSeqs {
		var diffs []int = []int{}
		j := 1
		for j < len(any(ls)) {
			diffs = append(_toAnySlice(diffs), any((ls[j] - ls[(j-1)])))
			j = (j + 1)
		}
		k := 0
		for k < (len(any(ls)) - 1) {
			fmt.Println(strings.TrimSuffix(fmt.Sprintln(any(fmt.Sprint(any(ls[k]))+" ("+fmt.Sprint(any(diffs[k]))+") "), func() int {
				if false {
					return 1
				}
				return 0
			}()), "\n"))
			k = (k + 1)
		}
		fmt.Println(strings.TrimSuffix(fmt.Sprintln(any(fmt.Sprint(any(ls[(len(any(ls))-1)])))), "\n"))
	}
	fmt.Println(strings.TrimSuffix(fmt.Sprintln(any("")), "\n"))
}

// line 77
func main() {
	fmt.Println(strings.TrimSuffix(fmt.Sprintln(any("For primes < 1 million:\n")), "\n"))
	for _, dir := range []string{"ascending", "descending"} {
		longestSeq(dir)
	}
}

var primes []int

func main() {
	primes = primesUpTo(LIMIT)
	LIMIT := 999999
	_ = LIMIT
	main()
}

func _toAnySlice[T any](s []T) []any {
	out := make([]any, len(s))
	for i, v := range s {
		out[i] = v
	}
	return out
}

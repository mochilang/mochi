//go:build slow && ignore

// Solution for SPOJ PRIME1 - Prime Generator
// https://www.spoj.com/problems/PRIME1
package main

import (
	"bufio"
	"fmt"
	"os"
)

// sieve returns all prime numbers up to limit using the simple sieve of Eratosthenes.
func sieve(limit int) []int {
	if limit < 2 {
		return nil
	}
	isComposite := make([]bool, limit+1)
	primes := []int{}
	for i := 2; i*i <= limit; i++ {
		if !isComposite[i] {
			for j := i * i; j <= limit; j += i {
				isComposite[j] = true
			}
		}
	}
	for i := 2; i <= limit; i++ {
		if !isComposite[i] {
			primes = append(primes, i)
		}
	}
	return primes
}

func main() {
	in := bufio.NewReader(os.Stdin)
	out := bufio.NewWriter(os.Stdout)
	defer out.Flush()

	var t int
	fmt.Fscan(in, &t)
	primes := sieve(100000) // sqrt(1e9) ≈ 31623; 100000 is safe upper bound
	for tc := 0; tc < t; tc++ {
		var m, n int
		fmt.Fscan(in, &m, &n)
		if m < 2 {
			m = 2
		}
		size := n - m + 1
		isComposite := make([]bool, size)
		for _, p := range primes {
			if p*p > n {
				break
			}
			start := (m + p - 1) / p * p
			if start < p*p {
				start = p * p
			}
			for j := start; j <= n; j += p {
				isComposite[j-m] = true
			}
		}
		for i := 0; i < size; i++ {
			if !isComposite[i] {
				fmt.Fprintln(out, m+i)
			}
		}
		if tc != t-1 {
			fmt.Fprintln(out)
		}
	}
}

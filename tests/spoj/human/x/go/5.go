//go:build slow && ignore

// Solution for SPOJ PALIN - The Next Palindrome
// https://www.spoj.com/problems/PALIN
package main

import (
	"bufio"
	"fmt"
	"os"
)

func nextPal(s []byte) string {
	n := len(s)
	all9 := true
	for _, c := range s {
		if c != '9' {
			all9 = false
			break
		}
	}
	if all9 {
		res := make([]byte, n+1)
		res[0] = '1'
		res[n] = '1'
		for i := 1; i < n; i++ {
			res[i] = '0'
		}
		return string(res)
	}
	i := n/2 - 1
	j := (n + 1) / 2
	for i >= 0 && s[i] == s[j] {
		i--
		j++
	}
	leftSmaller := false
	if i < 0 || s[i] < s[j] {
		leftSmaller = true
	}
	for k := 0; k < n/2; k++ {
		s[n-1-k] = s[k]
	}
	if leftSmaller {
		carry := byte(1)
		mid := n / 2
		if n%2 == 1 {
			if s[mid] == '9' {
				s[mid] = '0'
			} else {
				s[mid]++
				carry = 0
			}
			i = mid - 1
			j = mid + 1
		} else {
			i = mid - 1
			j = mid
		}
		for i >= 0 && carry > 0 {
			if s[i] == '9' {
				s[i] = '0'
				s[j] = '0'
			} else {
				s[i]++
				s[j] = s[i]
				carry = 0
			}
			i--
			j++
		}
		for i >= 0 {
			s[j] = s[i]
			i--
			j++
		}
	}
	return string(s)
}

func main() {
	in := bufio.NewReader(os.Stdin)
	out := bufio.NewWriter(os.Stdout)
	var t int
	fmt.Fscan(in, &t)
	for ; t > 0; t-- {
		var str string
		fmt.Fscan(in, &str)
		fmt.Fprintln(out, nextPal([]byte(str)))
	}
	out.Flush()
}

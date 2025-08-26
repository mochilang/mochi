//go:build slow && ignore

// Solution for SPOJ TEST - Life, the Universe, and Everything
// https://www.spoj.com/problems/TEST
package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	in := bufio.NewReader(os.Stdin)
	out := bufio.NewWriter(os.Stdout)
	for {
		var n int
		if _, err := fmt.Fscan(in, &n); err != nil {
			break
		}
		if n == 42 {
			break
		}
		fmt.Fprintln(out, n)
	}
	out.Flush()
}

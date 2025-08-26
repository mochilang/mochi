//go:build slow && ignore

// Solution for SPOJ SBSTR1 - Substring Check (Bug Funny)
// https://www.spoj.com/problems/SBSTR1
package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func main() {
	in := bufio.NewReader(os.Stdin)
	out := bufio.NewWriter(os.Stdout)
	for {
		var a, b string
		if _, err := fmt.Fscan(in, &a, &b); err != nil {
			break
		}
		if strings.Contains(a, b) {
			fmt.Fprintln(out, 1)
		} else {
			fmt.Fprintln(out, 0)
		}
	}
	out.Flush()
}

//go:build slow && ignore

// Solution for SPOJ CMPLS - Complete the Sequence!
// https://www.spoj.com/problems/CMPLS
package main

import (
	"bufio"
	"fmt"
	"os"
)

func allEqual(a []int) bool {
	for i := 1; i < len(a); i++ {
		if a[i] != a[0] {
			return false
		}
	}
	return true
}

func complete(seq []int, c int) []int {
	table := make([][]int, 0)
	row := append([]int{}, seq...)
	table = append(table, row)
	last := row
	for len(last) > 1 && !allEqual(last) {
		diff := make([]int, len(last)-1)
		for i := 0; i < len(last)-1; i++ {
			diff[i] = last[i+1] - last[i]
		}
		table = append(table, diff)
		last = diff
	}
	for k := 0; k < c; k++ {
		b := len(table) - 1
		val := table[b][len(table[b])-1]
		table[b] = append(table[b], val)
		for r := b - 1; r >= 0; r-- {
			v := table[r][len(table[r])-1] + table[r+1][len(table[r+1])-1]
			table[r] = append(table[r], v)
		}
	}
	res := make([]int, c)
	for i := 0; i < c; i++ {
		res[i] = table[0][len(seq)+i]
	}
	return res
}

func main() {
	in := bufio.NewReader(os.Stdin)
	out := bufio.NewWriter(os.Stdout)
	var t int
	if _, err := fmt.Fscan(in, &t); err != nil {
		return
	}
	for caseIdx := 0; caseIdx < t; caseIdx++ {
		var s, c int
		fmt.Fscan(in, &s, &c)
		seq := make([]int, s)
		for i := 0; i < s; i++ {
			fmt.Fscan(in, &seq[i])
		}
		ext := complete(seq, c)
		for i, v := range ext {
			if i > 0 {
				fmt.Fprint(out, " ")
			}
			fmt.Fprint(out, v)
		}
		if caseIdx < t-1 {
			fmt.Fprintln(out)
		}
	}
	out.Flush()
}

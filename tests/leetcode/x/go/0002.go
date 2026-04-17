package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func addLists(a, b []int) []int {
	out := make([]int, 0, len(a)+len(b)+1)
	i, j, carry := 0, 0, 0
	for i < len(a) || j < len(b) || carry > 0 {
		sum := carry
		if i < len(a) { sum += a[i]; i++ }
		if j < len(b) { sum += b[j]; j++ }
		out = append(out, sum%10)
		carry = sum / 10
	}
	return out
}

func format(arr []int) string {
	parts := make([]string, len(arr))
	for i, v := range arr { parts[i] = fmt.Sprint(v) }
	return "[" + strings.Join(parts, ",") + "]"
}

func main() {
	in := bufio.NewReader(os.Stdin)
	var t int
	if _, err := fmt.Fscan(in, &t); err != nil { return }
	out := bufio.NewWriter(os.Stdout)
	defer out.Flush()
	for tc := 0; tc < t; tc++ {
		var n, m int
		fmt.Fscan(in, &n)
		a := make([]int, n)
		for i := 0; i < n; i++ { fmt.Fscan(in, &a[i]) }
		fmt.Fscan(in, &m)
		b := make([]int, m)
		for i := 0; i < m; i++ { fmt.Fscan(in, &b[i]) }
		fmt.Fprintln(out, format(addLists(a, b)))
	}
}

package main

import (
	"bufio"
	"fmt"
	"os"
)

func isSelfCrossing(x []int) bool {
	for i := 3; i < len(x); i++ {
		if x[i] >= x[i-2] && x[i-1] <= x[i-3] { return true }
		if i >= 4 && x[i-1] == x[i-3] && x[i]+x[i-4] >= x[i-2] { return true }
		if i >= 5 && x[i-2] >= x[i-4] && x[i]+x[i-4] >= x[i-2] && x[i-1] <= x[i-3] && x[i-1]+x[i-5] >= x[i-3] { return true }
	}
	return false
}

func main() {
	in := bufio.NewReader(os.Stdin); out := bufio.NewWriter(os.Stdout); defer out.Flush()
	var t int; if _, err := fmt.Fscan(in, &t); err != nil { return }
	for tc := 0; tc < t; tc++ {
		var n int; fmt.Fscan(in, &n); x := make([]int, n); for i := range x { fmt.Fscan(in, &x[i]) }
		if tc > 0 { fmt.Fprintln(out); fmt.Fprintln(out) }
		if isSelfCrossing(x) { fmt.Fprint(out, "true") } else { fmt.Fprint(out, "false") }
	}
}

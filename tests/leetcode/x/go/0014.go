package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func lcp(strs []string) string {
	prefix := strs[0]
	for {
		ok := true
		for _, s := range strs {
			if !strings.HasPrefix(s, prefix) {
				ok = false
				break
			}
		}
		if ok {
			return prefix
		}
		prefix = prefix[:len(prefix)-1]
	}
}

func main() {
	in := bufio.NewReader(os.Stdin)
	var t int
	if _, err := fmt.Fscan(in, &t); err != nil { return }
	out := bufio.NewWriter(os.Stdout)
	defer out.Flush()
	for tc := 0; tc < t; tc++ {
		var n int
		fmt.Fscan(in, &n)
		strs := make([]string, n)
		for i := range strs { fmt.Fscan(in, &strs[i]) }
		fmt.Fprintf(out, "\"%s\"\n", lcp(strs))
	}
}

package main

import (
	"bufio"
	"fmt"
	"os"
)

var values = map[byte]int{'I': 1, 'V': 5, 'X': 10, 'L': 50, 'C': 100, 'D': 500, 'M': 1000}

func romanToInt(s string) int {
	total := 0
	for i := 0; i < len(s); i++ {
		cur := values[s[i]]
		next := 0
		if i+1 < len(s) {
			next = values[s[i+1]]
		}
		if cur < next {
			total -= cur
		} else {
			total += cur
		}
	}
	return total
}

func main() {
	in := bufio.NewReader(os.Stdin)
	var t int
	if _, err := fmt.Fscan(in, &t); err != nil {
		return
	}
	out := bufio.NewWriter(os.Stdout)
	defer out.Flush()
	for i := 0; i < t; i++ {
		var s string
		fmt.Fscan(in, &s)
		fmt.Fprintln(out, romanToInt(s))
	}
}

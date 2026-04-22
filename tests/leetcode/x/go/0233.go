package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

func countDigitOne(n int64) int64 {
	var total int64
	for m := int64(1); m <= n; m *= 10 {
		high := n / (m * 10)
		cur := (n / m) % 10
		low := n % m
		if cur == 0 {
			total += high * m
		} else if cur == 1 {
			total += high*m + low + 1
		} else {
			total += (high + 1) * m
		}
	}
	return total
}

func main() {
	sc := bufio.NewScanner(os.Stdin)
	if !sc.Scan() {
		return
	}
	t, _ := strconv.Atoi(sc.Text())
	out := make([]string, 0, t)
	for i := 0; i < t && sc.Scan(); i++ {
		n, _ := strconv.ParseInt(sc.Text(), 10, 64)
		out = append(out, strconv.FormatInt(countDigitOne(n), 10))
	}
	w := bufio.NewWriter(os.Stdout)
	defer w.Flush()
	for i, v := range out {
		if i > 0 {
			fmt.Fprintln(w)
		}
		fmt.Fprint(w, v)
	}
}

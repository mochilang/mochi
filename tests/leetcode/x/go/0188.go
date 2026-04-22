package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

func solve(k int, prices []int) int {
	n := len(prices)
	if k >= n/2 {
		best := 0
		for i := 1; i < n; i++ {
			if prices[i] > prices[i-1] {
				best += prices[i] - prices[i-1]
			}
		}
		return best
	}
	const negInf = -1 << 60
	buy := make([]int, k+1)
	sell := make([]int, k+1)
	for i := 1; i <= k; i++ {
		buy[i] = negInf
	}
	for _, price := range prices {
		for t := 1; t <= k; t++ {
			if sell[t-1]-price > buy[t] {
				buy[t] = sell[t-1] - price
			}
			if buy[t]+price > sell[t] {
				sell[t] = buy[t] + price
			}
		}
	}
	return sell[k]
}

func main() {
	sc := bufio.NewScanner(os.Stdin)
	sc.Split(bufio.ScanWords)
	nextInt := func() int { sc.Scan(); v, _ := strconv.Atoi(sc.Text()); return v }
	if !sc.Scan() {
		return
	}
	t, _ := strconv.Atoi(sc.Text())
	w := bufio.NewWriter(os.Stdout)
	defer w.Flush()
	for tc := 0; tc < t; tc++ {
		k := nextInt()
		n := nextInt()
		prices := make([]int, n)
		for i := 0; i < n; i++ {
			prices[i] = nextInt()
		}
		if tc > 0 {
			fmt.Fprintln(w)
		}
		fmt.Fprint(w, solve(k, prices))
	}
}

//go:build ignore

package main

import "fmt"

func main() {
	nums := []int{1, 2, 3}
	letters := []string{"A", "B"}

	type Pair struct {
		N int
		L string
	}
	var pairs []Pair

	for _, n := range nums {
		if n%2 == 0 {
			for _, l := range letters {
				pairs = append(pairs, Pair{n, l})
			}
		}
	}

	fmt.Println("--- Even pairs ---")
	for _, p := range pairs {
		fmt.Println(p.N, p.L)
	}
}

//go:build ignore

package main

import (
	"fmt"
)

func main() {
	var nums []int = []int{1, 2, 3}
	var letters []string = []string{"A", "B"}
	_ = letters
	var pairs []Pairs = func() []Pairs {
		results := []Pairs{}
		for _, n := range nums {
			if (n % 2) == 0 {
				for _, l := range letters {
					results = append(results, Pairs{
						n,
						l,
					})
				}
			}
		}
		return results
	}()
	fmt.Println("--- Even pairs ---")
	for _, p := range pairs {
		fmt.Println(p.N, p.L)
	}
}

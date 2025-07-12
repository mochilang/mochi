//go:build ignore

package main

import (
	"fmt"
)

func main() {
	nums := []int{1, 2}
	letters := []string{"A", "B"}
	_ = letters
	bools := []bool{true, false}
	_ = bools
	combos := func() []Combos {
		results := []Combos{}
		for _, n := range nums {
			for _, l := range letters {
				for _, b := range bools {
					results = append(results, Combos{
						n,
						l,
						b,
					})
				}
			}
		}
		return results
	}()
	fmt.Println("--- Cross Join of three lists ---")
	for _, c := range combos {
		fmt.Println(c.N, c.L, c.B)
	}
}

//go:build ignore

package main

import (
	"fmt"
)

func main() {
	var nums []int = []int{1, 2}
	var letters []string = []string{"A", "B"}
	_ = letters
	var bools []bool = []bool{true, false}
	_ = bools
	var combos []Combos = func() []Combos {
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

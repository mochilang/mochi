//go:build ignore

package main

import "fmt"

func main() {
	nums := []int{1, 2}
	letters := []string{"A", "B"}
	bools := []bool{true, false}

	type Combo struct {
		N int
		L string
		B bool
	}
	var combos []Combo

	for _, n := range nums {
		for _, l := range letters {
			for _, b := range bools {
				combos = append(combos, Combo{n, l, b})
			}
		}
	}

	fmt.Println("--- Cross Join of three lists ---")
	for _, c := range combos {
		fmt.Println(c.N, c.L, c.B)
	}
}
